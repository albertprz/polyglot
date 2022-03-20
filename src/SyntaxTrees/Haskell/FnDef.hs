module SyntaxTrees.Haskell.FnDef  where

import SyntaxTrees.Haskell.Common ( Var, Literal, Type, TypeParam, Class, Pattern, AnyKindedType )
import Data.Set (Set)


data FnDef = FnDef {
    name  :: Var
  , sig   :: FnSig
  , exprs :: [FnExpr]
}

data LocalFnDef = LocalFnDef {
    name  :: Var
  , exprs :: [FnExpr]
}

data ClassDef = ClassDef {
    name        :: Class
  , constraints :: [ClassConstraint]
  , typeParam   :: TypeParam
  , sigs        :: [FnSig]
}

data InstanceDef = InstanceDef {
    class' :: Class
  , type'  :: AnyKindedType
  , defs   :: LocalFnDef
}


data ClassConstraint = ClassConstraint Class Type


data FnSig = FnSig {
    name        :: Var
  , constraints :: [ClassConstraint]
  , types       :: [Type]
  , paramTypes  :: Set TypeParam
}

data FnExpr = FnExpr {
     args :: [Var]
  ,  body :: FnBody
}

data FnBody =
    FnApply {
    fn   :: Var
  , args :: [Var]
} | LambdaExpr {
    args :: [Var]
  , body :: FnBody
} | LetExpr {
    fnBindings :: [LocalFnDef]
  , body       :: FnBody
} | IfExpr {
    cond       :: FnBody
  , ifBranch   :: FnBody
  , elseBranch :: FnBody
} | MultiWayIfExpr {
    whenExprs :: [WhenExpr]
  , otherwiseBranch :: FnBody
} | DoExpr {
    steps :: [DoStep]
  , last  :: FnBody
} | CaseOfExpr {
    cases :: [CaseBinding]
} | Var Var
  | Lit Literal

data WhenExpr = WhenExpr {
    cond       :: FnBody
  , ifBranch   :: FnBody
}

data DoStep = DoBinding Var FnBody | Body FnBody
data CaseBinding = CaseBinding Pattern FnBody
