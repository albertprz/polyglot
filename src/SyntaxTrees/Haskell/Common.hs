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
  | BoolLit Bool
  | IntLit String
  | FloatLit String
  | CharLit Char
  | StringLit String
  deriving (Eq, Show)


data QVar
  = QVar (Maybe Module) Var
  deriving (Eq, Show)

data QCtor
  = QCtor (Maybe Module) Ctor
  deriving (Eq, Show)

data QVarOp
  = QVarOp (Maybe Module) VarOp
  deriving (Eq, Show)

data QCtorOp
  = QCtorOp (Maybe Module) CtorOp
  deriving (Eq, Show)

data QClass
  = QClass (Maybe Module) Class
  deriving (Eq, Show)
