module SyntaxTrees.Purescript.FnDef where

import Data.List                      (intercalate)
import SyntaxTrees.Purescript.Common  (Literal, QCtor, QCtorOp, QVar, QVarOp,
                                       Var, VarOp)
import SyntaxTrees.Purescript.Pattern (Pattern, showPatternNested)
import SyntaxTrees.Purescript.Type    (Type)
import Utils.List                     (mix)
import Utils.String                   (Wrapper (Wrapper), joinWords, str,
                                       wrapBlock, wrapContext, wrapCsv,
                                       wrapCurlyCsv, wrapParens, wrapSpaces,
                                       wrapSquareCsv, (+++))



data FnSig
  = FnSig
      { name  :: Var
      , type' :: Type
      }

data FnDef
  = FnDef
      { names :: [Var]
      , args  :: [Pattern]
      , body  :: MaybeGuardedFnBody
      }

data InfixFnDef
  = InfixFnDef
      { associativity :: Associativity
      , precedence    :: Integer
      , fnName        :: Var
      , opName        :: VarOp
      }

data FnDefOrSig
  = Def FnDef
  | Sig FnSig

data FnBody
  = FnApply
      { fn   :: FnBody
      , args :: [FnBody]
      }
  | InfixFnApply
      { fnOps :: [FnOp]
      , args  :: [FnBody]
      }
  | LeftOpSection
      { fnOp :: FnOp
      , arg  :: FnBody
      }
  | RightOpSection
      { arg  :: FnBody
      , fnOp :: FnOp
      }
  | LambdaExpr
      { patterns :: [Pattern]
      , body     :: FnBody
      }
  | LetExpr
      { fnBindings :: [FnDefOrSig]
      , body       :: FnBody
      }
  | WhereExpr
      { body       :: FnBody
      , fnBindings :: [FnDefOrSig]
      }
  | IfExpr
      { cond       :: FnBody
      , ifBranch   :: FnBody
      , elseBranch :: FnBody
      }
  | MultiWayIfExpr
      { whenExprs :: [GuardedFnBody]
      }
  | DoExpr
      { steps :: [DoStep]
      }
  | CaseOfExpr
      { matchee :: FnBody
      , cases   :: [CaseBinding]
      }
  | LambdaCaseExpr
      { cases :: [CaseBinding]
      }
  | RecordCreate
      { ctor        :: QCtor
      , namedFields :: [(Var, FnBody)]
      }
  | RecordUpdate
      { var         :: FnBody
      , namedFields :: [(Var, FnBody)]
      }
  | TypeAnnotation FnBody Type
  | ArrayRange FnBody FnBody
  | Tuple [FnBody]
  | Array [FnBody]
  | FnOp' FnOp
  | FnVar' FnVar
  | Literal' Literal

data FnVar
  = Selector Var
  | Selection QVar [Var]
  | Var' QVar
  | Ctor' QCtor

data FnOp
  = VarOp' QVarOp
  | CtorOp' QCtorOp

data DoStep
  = DoBinding [Var] FnBody
  | LetBinding [FnDefOrSig]
  | Body FnBody

data CaseBinding
  = CaseBinding Pattern MaybeGuardedFnBody

data MaybeGuardedFnBody
  = Guarded [GuardedFnBody]
  | Standard FnBody

data GuardedFnBody
  = GuardedFnBody
      { guard :: Guard
      , body  :: FnBody
      }

data Guard
  = Guard [PatternGuard]
  | Otherwise

data PatternGuard
  = PatternGuard Pattern FnBody
  | SimpleGuard FnBody

data Associativity
  = LAssoc
  | RAssoc


instance Show FnSig where
  show (FnSig x y) =
    joinWords [show x,
               "::",
               show y]

instance Show FnDef where
  show (FnDef x y z) =
    joinWords [wrapCsv x,
               intercalate " " (showPatternNested <$> y),
               showMaybeGuardedFnBody "=" z]

instance Show InfixFnDef where
  show (InfixFnDef x y z t) =
    joinWords [show x,
               show y,
               show z,
               "as",
               show t]

instance Show FnDefOrSig where
  show (Def x) = show x
  show (Sig x) = show x

instance Show FnBody where

  show (FnApply fn args) =
    joinWords [showNestedFnBody fn,
               intercalate " " $ showNestedFnBody <$> args]

  show (InfixFnApply fnOps args) =
    intercalate "" $ mix (showNestedInfixFnBody <$> args)
                         (wrapSpaces . show <$> fnOps)

  show (LeftOpSection x y) =
    joinWords ["_",
              show x,
              showNestedFnBody y]

  show (RightOpSection x y) =
    joinWords [showNestedFnBody x,
               show y,
               "_"]

  show (LambdaExpr x y) =
    joinWords ["\\" ++ intercalate " " (showPatternNested <$> x),
               "->",
               show y]

  show (LetExpr x y) =
    "\n" ++ wrapContext ("let" ++ wrapBlock x ++ "in" ++ "\n" ++ show y)

  show (WhereExpr x y) =
    show x ++ "\n" ++ wrapContext ("where" ++ wrapBlock y)

  show (IfExpr x y z) =
    "if" +++ show x +++
    "then" ++ wrapBlock [y] ++ "\n" ++
    "else" ++ wrapBlock [z]

  show (MultiWayIfExpr x) =
    "if" ++ wrapBlock (Wrapper . ("|" ++) .
            showGuardedFnBody "->" <$> x)

  show (DoExpr x) =
    "do" ++ wrapBlock x

  show (CaseOfExpr x y) =
    joinWords ["case",
              show x,
              "of",
              wrapBlock y]

  show (LambdaCaseExpr x) =
    joinWords ["case",
               "_",
               "of",
               wrapBlock x]

  show (RecordCreate x y) =
    joinWords [show x,
               wrapCurlyCsv $ Wrapper . format <$> y]
    where
      format (z, t) = show z ++ ":" +++ show t

  show (RecordUpdate x y) =
    joinWords [show x,
               wrapCurlyCsv $ Wrapper . format <$> y]
    where
      format (z, t) = show z +++ "=" +++ show t

  show (TypeAnnotation x y) = joinWords [show x, "::", show y]
  show (ArrayRange x y) = joinWords [show x, "..", show y]
  show (Tuple x) = wrapParens $ str (wrapSpaces "/\\") x
  show (Array x) = wrapSquareCsv x
  show (FnOp' x) = wrapParens $ show x
  show (FnVar' x) = show x
  show (Literal' x) = show x


instance Show FnVar where
  show (Selector x)    = "." ++ show x
  show (Selection x y) = intercalate "." (show x : (show <$> y))
  show (Var' x)        = show x
  show (Ctor' x)       = show x

instance Show FnOp where
  show (VarOp' x)  = show x
  show (CtorOp' x) = show x

instance Show DoStep where

  show (DoBinding x y) =
    joinWords [wrapCsv x,
               "<-",
               show y]

  show (LetBinding x) = "let" ++ wrapBlock x

  show (Body x) = show x

instance Show CaseBinding where
  show (CaseBinding x y) =
    joinWords [showPatternNested x,
               showMaybeGuardedFnBody "->" y]

instance Show Guard where
  show (Guard x)   = str ("\n" ++ ",") x
  show (Otherwise) = "otherwise"

instance Show PatternGuard where
  show (PatternGuard x y) =
    joinWords [show x,
               "<-",
               show y]
  show (SimpleGuard x) = show x

instance Show Associativity where
  show LAssoc = "infixl"
  show RAssoc = "infixr"


showMaybeGuardedFnBody :: String -> MaybeGuardedFnBody -> String
showMaybeGuardedFnBody op (Guarded x) =
  wrapBlock $ Wrapper . ("|" ++) . showGuardedFnBody op <$> x
showMaybeGuardedFnBody op (Standard x) = op +++ show x

showGuardedFnBody :: String -> GuardedFnBody -> String
showGuardedFnBody op (GuardedFnBody x y) =
  joinWords [show x,
            op,
            show y]

showNestedFnBody :: FnBody -> String
showNestedFnBody x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      FnApply _ _        -> True
      InfixFnApply _ _   -> True
      LeftOpSection _ _  -> True
      RightOpSection _ _ -> True
      LambdaExpr _ _     -> True
      LetExpr _ _        -> True
      WhereExpr _ _      -> True
      RecordCreate _ _   -> True
      RecordUpdate _ _   -> True
      ArrayRange _ _     -> True
      TypeAnnotation _ _ -> True
      _                  -> False

showNestedInfixFnBody :: FnBody -> String
showNestedInfixFnBody x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      InfixFnApply _ _   -> True
      LeftOpSection _ _  -> True
      RightOpSection _ _ -> True
      LambdaExpr _ _     -> True
      LetExpr _ _        -> True
      WhereExpr _ _      -> True
      RecordCreate _ _   -> True
      RecordUpdate _ _   -> True
      ArrayRange _ _     -> True
      TypeAnnotation _ _ -> True
      _                  -> False
