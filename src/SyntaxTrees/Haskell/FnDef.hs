module SyntaxTrees.Haskell.FnDef  where

import SyntaxTrees.Haskell.Common ( Var, Literal )
import SyntaxTrees.Haskell.Type ( Type )
import SyntaxTrees.Haskell.Pattern ( Pattern )


data FnDef = FnDef {
    name  :: Var
  , exprs :: [FnExpr]
}

data FnSig = FnSig {
    name  :: Var
  , type' :: Type
}

data FnDefOrSig = Def FnDef | Sig FnSig

data FnExpr = FnExpr {
     args       :: [Pattern]
  ,  guardedFns :: [GuardedFnBody]
}

data FnBody =
    FnApply {
    fn   :: Var
  , args :: [Var]
} | LambdaExpr {
    args :: [Var]
  , body :: FnBody
} | LetExpr {
    fnBindings :: [FnDefOrSig]
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
} | Tuple
  | List
  | Var Var
  | Lit Literal

data WhenExpr = WhenExpr {
    cond       :: FnBody
  , ifBranch   :: FnBody
}

data GuardedFnBody = GuardedFnBody {
    guards :: [FnBody]
  , body :: FnBody
}

data DoStep = DoBinding Var FnBody | Body FnBody
data CaseBinding = CaseBinding Pattern GuardedFnBody
