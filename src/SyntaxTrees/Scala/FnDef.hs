module SyntaxTrees.Scala.FnDef where

import Data.Foldable             (Foldable (fold))
import Data.Monoid.HT            (when)
import SyntaxTrees.Scala.Common  (Literal, Modifier, QCtor, QCtorOp, QVar,
                                  QVarOp, Var, Wrapper (..))
import SyntaxTrees.Scala.Pattern (Pattern)
import SyntaxTrees.Scala.Type    (ArgList, Type, TypeParam, UsingArgList)

import Data.List (intercalate)

import Utils.Foldable (hasSome, wrapMaybe)
import Utils.String   (joinMaybe, joinWords, str, wrapBlock, wrapLetContext,
                       wrapParens, wrapParensCsv, wrapSingleBlock,
                       wrapSpacedBlock, wrapSpaces, wrapSquareCsv, (+++))


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
      { qualifiers   :: [Modifier]
      , name         :: Maybe Var
      , typeParams   :: [TypeParam]
      , usingArgs    :: [UsingArgList]
      , returnType   :: Type
      , internalDefs :: [InternalFnDef]
      }


data FnBody
  = FnApply
      { fn   :: FnBody
      , args :: [FnBody]
      }
  | InfixFnApply
      { fnOp :: FnOp
      , args :: [FnBody]
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
  show (FnSig x y z t) = ("" `joinMaybe` (Wrapper <$> wrapMaybe
                         (joinWords [wrapSquareCsv x,
                                    str " " y,
                                    str " " z])))
                         ++ ":" `joinMaybe` t

instance Show ValDef where
  show (ValDef x y z t) = joinWords [str " " x,
                                     "val",
                                     showVal y z t]

instance Show MethodDef where
  show (MethodDef x y z t) = joinWords [str " " x,
                                        "def",
                                        showDef y z t]

instance Show GivenDef where
  show (GivenDef x y z t u v) = joinWords [str " " x,
                                          "given",
                                          showGiven y z t u v]

instance Show FnBody where
  show (FnApply x y)      = joinWords [show x, wrapParensCsv y]
  show (InfixFnApply x [y]) = showForInfix y +++ show x
  show (InfixFnApply x y) = str (wrapSpaces $ show x) (Wrapper . showForInfix <$> y)
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
             -> Type -> [InternalFnDef] -> String
showGiven x y z t u = joinWords [foldMap show x,
                                 wrapSquareCsv y,
                                 str " " z,
                                 when displaySep ":",
                                 show t,
                                 "with",
                                 wrapSpacedBlock u]
  where
    displaySep = hasSome x || hasSome y || hasSome z


showTuple :: Show a => [a] -> String
showTuple [x] = show x
showTuple x   = wrapParens $ str ", " x
