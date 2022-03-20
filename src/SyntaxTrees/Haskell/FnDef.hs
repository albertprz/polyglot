module SyntaxTrees.Haskell.FnDef  where

import SyntaxTrees.Haskell.Common ( Term, Literal, Type, TypeParameter, Class, Pattern )


data FnDef = FnDef {
    name  :: Term
  , sig   :: FnSig
  , exprs :: [FnExpr]
}

data LocalFnDef = LocalFnDef {
    name  :: Term
  , exprs :: [FnExpr]
}

data FnSig = FnSig {
    name        :: Term
  , constraints :: [ClassConstraint]
  , types       :: [Type]
  , paramTypes  :: [TypeParameter]
}


data ClassDef = ClassDef {
    name        :: Class
  , constraints :: [ClassConstraint]
  , typeParam   :: TypeParameter
  , sigs        :: [FnSig]
}

data InstanceDef = InstanceDef {
    class' :: Class
  , type'  :: Type
  , defs   :: LocalFnDef
}

data ClassConstraint = ClassConstraint Class Type



data FnExpr = FnExpr {
     args :: [Term]
  ,  body :: FnBody
}

data FnBody =
    FnApply {
    name :: Term
  , args :: [Term]
} | LambdaExpr {
    args :: [Term]
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
} | Var Term
  | Lit Literal

data WhenExpr = WhenExpr {
    cond       :: FnBody
  , ifBranch   :: FnBody
}

data DoStep = DoBinding Term FnBody | Body FnBody
data CaseBinding = CaseBinding Pattern FnBody
