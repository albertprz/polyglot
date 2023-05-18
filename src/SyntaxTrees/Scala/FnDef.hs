module SyntaxTrees.Scala.FnDef where

import Data.Foldable             (Foldable (fold))
import Data.Monoid.HT            (when)
import SyntaxTrees.Scala.Common  (Literal, Modifier, QCtor, QCtorOp, QVar,
                                  QVarOp, Var)
import SyntaxTrees.Scala.Pattern (Pattern)
import SyntaxTrees.Scala.Type    (ArgList, Type, TypeParam,
                                  UsingArgList (UsingArgList))

import Data.List (intercalate)

import Utils.Foldable (hasSome, wrapMaybe)
import Utils.String   (Wrapper (..), joinMaybe, joinWords, str, strs, wrapBlock,
                       wrapLetContext, wrapParens, wrapParensCsv,
                       wrapSingleBlock, wrapSpacedBlock, wrapSpaces,
                       wrapSquareCsv, (+++))


data FnSig
  = FnSig
      { typeParams :: [TypeParam]
      , argLists   :: [ArgList]
      , usingArgs  :: [UsingArgList]
      , returnType :: Maybe Type
      }

data ValDef
  = ValDef
      { qualifiers :: [Modifier]
      , names      :: [Var]
      , returnType :: Maybe Type
      , body       :: Maybe FnBody
      }

data MethodDef
  = MethodDef
      { qualifiers :: [Modifier]
      , name       :: Var
      , sig        :: Maybe FnSig
      , body       :: Maybe FnBody
      }

data GivenDef
  = GivenDef
      { qualifiers :: [Modifier]
      , name       :: Maybe Var
      , typeParams :: [TypeParam]
      , usingArgs  :: [UsingArgList]
      , returnType :: Type
      , bodyOrDefs :: Either FnBody [InternalFnDef]
      }


data FnBody
  = FnApply
      { fn   :: FnBody
      , args :: [FnBody]
      }
  | NamedFnApply
      { fn        :: FnBody
      , namedArgs :: [(Var, FnBody)]
      }
  | InfixFnApply
      { fnOps :: [FnOp]
      , args  :: [FnBody]
      }
  | LambdaExpr
      { vars :: [Var]
      , body :: FnBody
      }
  | LetExpr
      { fnBindings :: [InternalFnDef]
      , body       :: FnBody
      }
  | IfExpr
      { cond       :: FnBody
      , ifBranch   :: FnBody
      , elseBranch :: FnBody
      }
  | CondExpr
      { whenBranches :: [WhenExpr]
      , elseBranch   :: FnBody
      }
  | ForExpr
      { steps :: [ForStep]
      , yield :: FnBody
      }
  | MatchExpr
      { matchee :: FnBody
      , cases   :: [CaseBinding]
      }
  | TopLevelMatchExpr
      { cases :: [CaseBinding]
      }
  | Tuple [FnBody]
  | FnVar' FnVar
  | Literal' Literal

data FnVar
  = Selection QVar [Var]
  | Var' QVar
  | Ctor' QCtor

data FnOp
  = VarOp' QVarOp
  | CtorOp' QCtorOp

data ForStep
  = ForBinding [Var] FnBody
  | LetBinding [InternalFnDef]
  | Condition FnBody

data CaseBinding
  = CaseBinding
      { pattern' :: Pattern
      , guard    :: Maybe FnBody
      , body     :: FnBody
      }

data InternalFnDef
  = FnVal ValDef
  | FnMethod MethodDef
  | FnGiven GivenDef

data WhenExpr
  = WhenExpr
      { cond :: FnBody
      , body :: FnBody
      }



instance Show FnSig where
  show (FnSig x y z t) =
    ("" `joinMaybe`
     (Wrapper <$> wrapMaybe (joinWords [wrapSquareCsv x,
                                        str " " y,
                                        str " " z])))
    ++ ":" `joinMaybe` t

instance Show ValDef where
  show (ValDef x y z t) =
    joinWords [str " " x,
               "val",
               showVal y z t]

instance Show MethodDef where
  show (MethodDef x y z t) =
    joinWords [str " " x,
               "def",
               showDef y z t]

instance Show GivenDef where
  show (GivenDef x y z t u v) =
    joinWords [str " " x,
               "given",
               showGiven y z t u v]

instance Show FnBody where
  show (FnApply x y)
    | (FnVar' (Ctor' _)) <- x = joinWords [show x, wrapParensCsv y]
    | otherwise = joinWords [show x,
                             joinWords $ wrapParens . show <$> y]
  show (NamedFnApply x y) = joinWords [show x, wrapParensCsv $ Wrapper .
                                      (\(a, b) -> show a +++ "=" +++ show b) <$> y]
  show (InfixFnApply x [y]) = showForInfix y +++ foldMap show x
  show (InfixFnApply x y) = strs (wrapSpaces . show <$> x)
                                 (showForInfix <$> y)
  show (LambdaExpr [] y)  = wrapParens $ show y
  show (LambdaExpr [x] y) = wrapParens $ joinWords [show x, "=>", show y]
  show (LambdaExpr x y)   = wrapParens $ joinWords [wrapParensCsv x, "=>", show y]
  show (LetExpr x y)      = wrapLetContext x y
  show (Tuple [x])        = show x
  show (Tuple x)          = wrapParensCsv x
  show (FnVar' x)         = show x
  show (Literal' x)       = show x
  show (IfExpr x y z)     = joinWords ["if", show x,
                                       "then", show y,
                                       "else", show z]
  show (CondExpr x y)     = str "\n  else " (Wrapper <$> ((show <$> x) ++ [show y]))
  show (ForExpr x y)      = joinWords ["for",
                                      wrapBlock x,
                                      "yield",
                                      show y]
  show (MatchExpr x y)    = joinWords [show x,
                                      "match",
                                      wrapBlock y]
  show (TopLevelMatchExpr x) = wrapBlock x

instance Show ForStep where
  show (ForBinding x y) = joinWords [showTuple x, "<-", show y]
  show (LetBinding x)   = unlines (showBinding <$> x)
  show (Condition x)    = joinWords ["if", show x]

instance Show CaseBinding where
  show (CaseBinding x y z) = joinWords ["case", show x,
                                        "if" `joinMaybe` y,
                                        "=>", show z]
instance Show WhenExpr where
  show (WhenExpr x y) = joinWords ["if", show x,
                                  "then", show y]

instance Show FnVar where
  show (Selection x y) = intercalate "." (show x : (show <$> y))
  show (Var' x)        = show x
  show (Ctor' x)       = show x

instance Show FnOp where
  show (VarOp' x)  = show x
  show (CtorOp' x) = show x

instance Show InternalFnDef where
  show (FnVal x)    = show x
  show (FnMethod x) = show x
  show (FnGiven x)  = show x


showBinding :: InternalFnDef -> String
showBinding (FnVal (ValDef _ y _ t))         = showVal y Nothing t
showBinding (FnMethod (MethodDef _ y _ t))   = showDef y Nothing t
showBinding (FnGiven (GivenDef _ y z t u v)) = showGiven y z t u v


showForInfix :: FnBody -> String
showForInfix x@(InfixFnApply _ _) = wrapParens $ show x
showForInfix x                    = show x


showVal :: [Var] -> Maybe Type -> Maybe FnBody -> String
showVal x y z = showTuple x ++  ":" `joinMaybe` y
                            +++ "=" `joinMaybe` z

showDef :: Var -> Maybe FnSig -> Maybe FnBody -> String
showDef x y z = show x ++ (fold $ show <$> y)
                       +++ "=" `joinMaybe` (Wrapper . wrapSingleBlock <$> z)


showGiven :: Maybe Var -> [TypeParam] -> [UsingArgList]
             -> Type -> Either FnBody [InternalFnDef] -> String

showGiven x y z t u = joinWords [foldMap show x,
                                 wrapSquareCsv y,
                                 str " " z,
                                 when displaySep ":",
                                 show t]
   ++ case u of
        Left body -> joinWords ["=",
                               show body]
        Right defs -> joinWords ["with",
                                wrapSpacedBlock defs]

  where
    displaySep = hasSome x || hasSome y || hasSome (foldMap fields z)
    fields (UsingArgList h) = h


showTuple :: Show a => [a] -> String
showTuple [x] = show x
showTuple x   = wrapParens $ str ", " x
