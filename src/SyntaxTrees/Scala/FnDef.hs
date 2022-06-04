module SyntaxTrees.Scala.FnDef where

import SyntaxTrees.Scala.Common  (Ctor, CtorOp, Literal, Modifier, Var, VarOp)
import SyntaxTrees.Scala.Pattern (Pattern)
import SyntaxTrees.Scala.Type    (ArgList, Type, TypeParam, UsingArgList)


data VarDef
  = VarDef
      { name       :: Var
      , qualifiers :: [Modifier]
      , body       :: Maybe FnBody
      }
  deriving (Show)

data ValDef
  = ValDef
      { qualifiers :: [Modifier]
      , name       :: Var
      , body       :: Maybe FnBody
      }
  deriving (Show)

data MethodDef
  = MethodDef
      { qualifiers :: [Modifier]
      , name       :: Var
      , sig        :: FnSig
      , body       :: Maybe FnBody
      }
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
  | DoExpr
      { steps :: [DoStep]
      }
  | MatchExpr
      { matchee :: FnBody
      , cases   :: [CaseBinding]
      }
  | Tuple [FnBody]
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
  deriving (Show)

data PatternGuard
  = PatternGuard Pattern FnBody
  | SimpleGuard FnBody
  | Otherwise
  deriving (Show)


data FnSig
  = FnSig
      { typeParams :: [TypeParam]
      , argLists   :: [ArgList]
      , usingArgs  :: UsingArgList
      , returnType :: Type
      }
  deriving (Show)

data InternalFnDef
  = FnVar VarDef
  | FnVal ValDef
  | FnMethod MethodDef
  deriving (Show)


