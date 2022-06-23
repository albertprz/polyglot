module SyntaxTrees.Haskell.FnDef where

import SyntaxTrees.Haskell.Common  (Ctor, CtorOp, Literal, Var, VarOp)
import SyntaxTrees.Haskell.Pattern (Pattern)
import SyntaxTrees.Haskell.Type    (Type)

data FnSig
  = FnSig
      { name  :: Var
      , type' :: Type
      }
  deriving (Show)

data FnDef
  = FnDef
      { name :: Var
      , args :: [Pattern]
      , body :: MaybeGuardedFnBody
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
  | Tuple [FnBody]
  | List [FnBody]
  | FnVar' FnVar
  | Literal' Literal
  deriving (Show)

data FnVar
  = Var' Var
  | Ctor' Ctor
  deriving (Show)

data FnOp
  = VarOp' VarOp
  | CtorOp' CtorOp
  deriving (Show)

data OperatorPosition
  = LeftPos
  | RightPos
  deriving (Show)

data DoStep
  = DoBinding Var FnBody
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
