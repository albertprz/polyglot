module SyntaxTrees.Scala.FnDef where

import SyntaxTrees.Scala.Common  (Ctor, CtorOp, Literal, Modifier, Var, VarOp)
import SyntaxTrees.Scala.Pattern (Pattern)
import SyntaxTrees.Scala.Type    (ArgList, Type, TypeParam, UsingArgList)
import Utils.String


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
      , sig        :: FnSig
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
  | MultiWayIfExpr
      { whenExprs :: [WhenExpr]
      , otherwise :: Maybe FnBody
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
  = Var' Var
  | Ctor' Ctor

data FnOp
  = VarOp' VarOp
  | CtorOp' CtorOp

data ForStep
  = ForBinding Var FnBody
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
  show (FnSig x y z t) = joinWords [wrapSquareCsv x,
                                    str " " y,
                                    str " " z,
                                    ":" `joinMaybe` t]

instance Show ValDef where
  show (ValDef x y z t) = joinWords [str " " x,
                                     "val",
                                     showDef y z t]

instance Show MethodDef where
  show (MethodDef x y z t) = joinWords [str " " x,
                                        "def",
                                        showDef y (Just z) t]

instance Show GivenDef where
  show (GivenDef x y z t u v) = joinWords [str " " x,
                                          "given",
                                          maybe "" show y,
                                          wrapSquareCsv z,
                                          str " " t,
                                          if displaySep then ":" else "",
                                          show u,
                                          "with",
                                          wrapSpacedBlock v]
    where
      displaySep = not (null y && null z && null t)


instance Show FnBody where
  show (FnApply x y)      = joinWords [show x, wrapParensCsv y]
  show (InfixFnApply x y) = str (show x) y
  show (LambdaExpr x y)   = joinWords [wrapParensCsv x, "=>", show y]
  show (LetExpr x y)      = wrapLetContext x y
  show (Tuple x)          = wrapParensCsv x
  show (FnVar' x)         = show x
  show (Literal' x)       = show x
  show (IfExpr x y z)     = joinWords ["if", show x,
                                       "then", show y,
                                       "else", show z]
  show (MultiWayIfExpr x y)     = joinWords [str ("\n" ++ "else ") x,
                                             ("\n" ++ "else ") `joinMaybe` y]
  show (ForExpr x y)      = joinWords ["for",
                                      wrapBlock x,
                                      "yield",
                                      show y]
  show (MatchExpr x y)    = joinWords [show x,
                                      "match",
                                      wrapBlock y]

instance Show ForStep where
  show (ForBinding x y) = joinWords [show x, "<-", show y]
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

showDef :: (Show a, Show b, Show c) => a -> Maybe b -> Maybe c -> String
showDef x y z = show x ++  ":" `joinMaybe` y
                       +++ "=" `joinMaybe` z
