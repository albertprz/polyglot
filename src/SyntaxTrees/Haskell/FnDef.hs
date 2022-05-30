module SyntaxTrees.Haskell.FnDef  where

import SyntaxTrees.Haskell.Common ( Var, Literal )
import SyntaxTrees.Haskell.Type ( Type )
import SyntaxTrees.Haskell.Pattern ( Pattern )


data FnSig = FnSig {
    name  :: Var
  , type' :: Type
}

data FnDef = FnDef {
     name          :: Var
  ,  args          :: [Pattern]
  ,  body          :: MaybeGuardedFnBody
}

data FnDefOrSig = Def FnDef | Sig FnSig


data FnBody =
    FnApply {
    fn   :: FnBody
  , args :: [FnBody]
} | OperatorFnApply {
    fn :: FnBody
  , args :: [FnBody]
} | LambdaExpr {
    vars :: [Var]
  , body :: FnBody
} | LetExpr {
    fnBindings :: [FnDefOrSig]
  , body       :: FnBody
} | WhereExpr {
    body       :: FnBody
  , fnBindings :: [FnDefOrSig]
} | IfExpr {
    cond       :: FnBody
  , ifBranch   :: FnBody
  , elseBranch :: FnBody
} | MultiWayIfExpr {
    whenExprs :: [GuardedFnBody]
} | DoExpr {
    steps :: [DoStep]
} | CaseOfExpr {
    matchee :: FnBody
  , cases   :: [CaseBinding]
} | Tuple [FnBody]
  | List [FnBody]
  | Var' Var
  | Literal' Literal


data DoStep = DoBinding Var FnBody
            | Body FnBody

data CaseBinding = CaseBinding Pattern MaybeGuardedFnBody



data MaybeGuardedFnBody = Guarded [GuardedFnBody]
                        | Standard FnBody

data GuardedFnBody = GuardedFnBody {
    guard :: Guard
  , body :: FnBody
}

data Guard = Guard [PatternGuard]

data PatternGuard = PatternGuard Pattern FnBody
                  | SimpleGuard FnBody
                  | Otherwise
