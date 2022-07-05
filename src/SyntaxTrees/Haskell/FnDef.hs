module SyntaxTrees.Haskell.FnDef where

import SyntaxTrees.Haskell.Common  (Literal, QCtor, QCtorOp, QVar, QVarOp, Var)
import SyntaxTrees.Haskell.Pattern (Pattern)
import SyntaxTrees.Haskell.Type    (Type)


-- TODO: Support Record Apply & Record Update syntax

data FnSig
  = FnSig
      { name  :: Var
      , type' :: Type
      }
  deriving (Show)

data FnDef
  = FnDef
      { names :: [Var]
      , args  :: [Pattern]
      , body  :: MaybeGuardedFnBody
      }
  deriving (Show)

data FnDefOrSig
  = Def FnDef
  | Sig FnSig
  deriving (Show)

data FnBody
  = FnApply
      { fn   :: FnBody
      , args :: [FnBody]
      }
  | InfixFnApply
      { fnOp :: FnOp
      , args :: [FnBody]
      }
  | LeftOpSection
      { fnOp :: FnOp
      , arg  :: FnBody
      }
  | RightOpSection
      { arg  :: FnBody
      , fnOp :: FnOp
      }
  | PostFixOpSection
      { arg  :: FnBody
      , fnOp :: FnOp
      }
  | LambdaExpr
      { vars :: [Var]
      , body :: FnBody
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
  | Tuple [FnBody]
  | List [FnBody]
  | FnVar' FnVar
  | Literal' Literal
  deriving (Show)

data FnVar
  = Selector Var
  | Selection QVar [Var]
  | Var' QVar
  | Ctor' QCtor
  deriving (Show)

data FnOp
  = VarOp' QVarOp
  | CtorOp' QCtorOp
  deriving (Show)

data OperatorPosition
  = LeftPos
  | RightPos
  deriving (Show)

data DoStep
  = DoBinding [Var] FnBody
  | LetBinding [FnDefOrSig]
  | Body FnBody
  deriving (Show)

data CaseBinding
  = CaseBinding Pattern MaybeGuardedFnBody
  deriving (Show)

data MaybeGuardedFnBody
  = Guarded [GuardedFnBody]
  | Standard FnBody
  deriving (Show)

data GuardedFnBody
  = GuardedFnBody
      { guard :: Guard
      , body  :: FnBody
      }
  deriving (Show)

data Guard
  = Guard [PatternGuard]
  | Otherwise
  deriving (Show)

data PatternGuard
  = PatternGuard Pattern FnBody
  | SimpleGuard FnBody
  deriving (Show)
