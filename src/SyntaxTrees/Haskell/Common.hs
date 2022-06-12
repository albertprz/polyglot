module SyntaxTrees.Haskell.Common where


newtype Var
  = Var String
  deriving (Eq, Show)

newtype Ctor
  = Ctor String
  deriving (Eq, Show)

newtype VarOp
  = VarOp String
  deriving (Eq, Show)

newtype CtorOp
  = CtorOp String
  deriving (Eq, Show)

newtype Class
  = Class String
  deriving (Eq, Show)

newtype Module
  = Module [String]
  deriving (Eq, Show)

data Literal
  = UnitLit
  | IntLit String
  | FloatLit String
  | CharLit String
  | StringLit String
  deriving (Eq, Show)
