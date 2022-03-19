module SyntaxTrees.Haskell.FnDef  where

import SyntaxTrees.Haskell.Term ( TermName, Literal )


data FnDef = FnDef {
    name :: TermName
  , sig :: FnSig
  , exprs :: [FnExpr]
}

data FnSig = FnSig {
    name :: TermName
  , types :: [TermName]
}

data FnExpr = FnExpr {
     args :: [TermName]
  ,  body :: FnBody
}

data FnBody = FnApply {
    name :: TermName
  , args :: [TermName]
} | LambdaExpr {
     args :: [TermName]
  ,  body :: FnBody
} | LetExpr {
     fnBindings :: [LocalFnDef]
  ,  body :: FnBody
} | IfExpr {
     cond :: FnBody
  ,  ifBranch :: FnBody
  ,  elseBranch :: FnBody
} | DoExpr {
     steps :: [DoStep]
  ,  last  :: FnBody
} | MatchExpr {
     cases :: [CaseBinding]
} | Var TermName
  | Lit Literal

data LocalFnDef = LocalFnDef {
    name :: TermName
  , exprs :: [FnExpr]
}

data DoStep = DoBinding TermName FnBody | Body FnBody

data CaseBinding = CaseBinding Pattern FnBody

data Pattern = CtorPattern TermName [TermName] |
               VarPattern TermName |
               LitPattern Literal

data Ctor = Ctor {
    ctorName :: TermName
  , fields :: TermName
}
