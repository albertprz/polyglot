module SyntaxTrees.Scala.Common where



newtype Var
  = Var String
  deriving (Show)

newtype Ctor
  = Ctor String
  deriving (Show)

newtype VarOp
  = VarOp String
  deriving (Show)

newtype CtorOp
  = CtorOp String
  deriving (Show)

newtype Package
  = Package String
  deriving (Show)


data Literal
  = UnitLit
  | IntLit String
  | FloatLit String
  | CharLit String
  | StringLit String
  deriving (Show)

data Modifier
  = Implicit
  | Lazy
  | Abstract
  | Private
  | Public
  | Protected
  | Override
  | Sealed
  | Open
  deriving (Show)
