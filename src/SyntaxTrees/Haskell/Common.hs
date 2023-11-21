module SyntaxTrees.Haskell.Common where

import ClassyPrelude


newtype Var
  = Var Text
  deriving (Eq)

newtype Ctor
  = Ctor Text
  deriving (Eq)

newtype VarOp
  = VarOp Text
  deriving (Eq)

newtype CtorOp
  = CtorOp Text
  deriving (Eq)

newtype Class
  = Class Text
  deriving (Eq)

newtype Module
  = Module [Text]
  deriving (Eq)

data Literal
  = UnitLit
  | BoolLit Bool
  | IntLit Text
  | FloatLit Text
  | CharLit Char
  | StringLit Text
  deriving (Eq)


data QVar
  = QVar (Maybe Module) Var
  deriving (Eq)

data QCtor
  = QCtor (Maybe Module) Ctor
  deriving (Eq)

data QVarOp
  = QVarOp (Maybe Module) VarOp
  deriving (Eq)

data QCtorOp
  = QCtorOp (Maybe Module) CtorOp
  deriving (Eq)

data QClass
  = QClass (Maybe Module) Class
  deriving (Eq)
