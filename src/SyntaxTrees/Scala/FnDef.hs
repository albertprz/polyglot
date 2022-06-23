module SyntaxTrees.Scala.FnDef where

import Data.Foldable             (Foldable (fold))
import Data.Monoid.HT            (when)
import SyntaxTrees.Scala.Common  (Literal, Modifier, QCtor, QCtorOp, QVar,
                                  QVarOp, Var, Wrapper (..))
import SyntaxTrees.Scala.Pattern (Pattern)
import SyntaxTrees.Scala.Type    (ArgList, Type, TypeParam, UsingArgList)

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
      , name       :: Var
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
      , internalDefs :: [MethodDef]
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
  = Var' QVar
  | Ctor' QCtor

data FnOp
  = VarOp' QVarOp
  | CtorOp' QCtorOp

data ForStep
  = ForBinding Var FnBody
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
                                          foldMap show y,
                                          wrapSquareCsv z,
                                          str " " t,
                                          when displaySep ":",
                                          show u,
                                          "with",
                                          wrapSpacedBlock v]
    where
      displaySep = hasSome y || hasSome z || hasSome t


instance Show FnBody where
  show (FnApply x y)      = joinWords [show x, wrapParensCsv y]
  show (InfixFnApply x [y]) = show y +++ show x
  show (InfixFnApply x y) = str (wrapSpaces $ show x) y
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
  show (ForExpr x y)      = joinWords ["for",
                                      wrapBlock x,
                                      "yield",
                                      show y]
  show (MatchExpr x y)    = joinWords [show x,
                                      "match",
                                      wrapBlock y]

instance Show ForStep where
  show (ForBinding x y) = joinWords [show x, "<-", show y]
  show (LetBinding x)   = unlines (drop 4 . show <$> x)
  show (Condition x)    = joinWords ["if", show x]

instance Show CaseBinding where
  show (CaseBinding x y z) = joinWords ["case", show x,
                                        "if" `joinMaybe` y,
                                        "=>", show z]
instance Show WhenExpr where
  show (WhenExpr x y) = joinWords ["if", show x,
                                  "then", show y]

instance Show FnVar where
  show (Var' x)  = show x
  show (Ctor' x) = show x

instance Show FnOp where
  show (VarOp' x)  = show x
  show (CtorOp' x) = show x

instance Show InternalFnDef where
  show (FnVal x)    = show x
  show (FnMethod x) = show x
  show (FnGiven x)  = show x


showVal :: Var -> Maybe Type -> Maybe FnBody -> String
showVal x y z = show x ++  ":" `joinMaybe` y
                       +++ "=" `joinMaybe` z

showDef :: Var -> Maybe FnSig -> Maybe FnBody -> String
showDef x y z = show x ++ (fold $ show <$> y)
                       +++ "=" `joinMaybe` (Wrapper . wrapSingleBlock <$> z)
