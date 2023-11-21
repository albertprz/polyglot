module SyntaxTrees.Haskell.FnDef where

import ClassyPrelude

import SyntaxTrees.Haskell.Common  (Literal, QCtor, QCtorOp, QVar, QVarOp, Var,
                                    VarOp)
import SyntaxTrees.Haskell.Pattern (Pattern)
import SyntaxTrees.Haskell.Type    (Type)



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

data InfixFnAnnotation
  = InfixFnAnnotation
      { associativity :: Associativity
      , precedence    :: Int
      , name          :: VarOp
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
  | ListRange FnBody (Maybe FnBody)
  | Tuple [FnBody]
  | List [FnBody]
  | FnVar' FnVar
  | FnOp' FnOp
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
