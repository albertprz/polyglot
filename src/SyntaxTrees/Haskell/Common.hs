module SyntaxTrees.Haskell.Common where


newtype Var
  = Var String
  deriving (Eq)

newtype Ctor
  = Ctor String
  deriving (Eq)

newtype VarOp
  = VarOp String
  deriving (Eq)

newtype CtorOp
  = CtorOp String
  deriving (Eq)

newtype Class
  = Class String
  deriving (Eq)

newtype Module
  = Module [String]
  deriving (Eq)

data Literal
  = UnitLit
  | BoolLit Bool
  | IntLit String
  | FloatLit String
  | CharLit Char
  | StringLit String
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
