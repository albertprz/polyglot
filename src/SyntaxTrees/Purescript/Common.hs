module SyntaxTrees.Purescript.Common where

import Data.List (intercalate)


newtype Var
  = Var String

newtype Ctor
  = Ctor String

newtype VarOp
  = VarOp String

newtype CtorOp
  = CtorOp String

newtype Class
  = Class String

newtype Module
  = Module [String]

data Literal
  = UnitLit
  | BoolLit Bool
  | IntLit String
  | NumberLit String
  | CharLit Char
  | StringLit String


data QVar
  = QVar (Maybe Module) Var

data QCtor
  = QCtor (Maybe Module) Ctor

data QVarOp
  = QVarOp (Maybe Module) VarOp

data QCtorOp
  = QCtorOp (Maybe Module) CtorOp

data QClass
  = QClass (Maybe Module) Class



instance Show Var where
  show (Var x) = x

instance Show Ctor where
  show (Ctor x) = x

instance Show VarOp where
  show (VarOp x) = x

instance Show CtorOp where
  show (CtorOp x) = x

instance Show Class where
  show (Class x) = x

instance Show Module where
  show (Module x) = intercalate "." x

instance Show Literal where
  show UnitLit         = "unit"
  show (BoolLit True)  = "true"
  show (BoolLit False) = "false"
  show (IntLit x)      = x
  show (NumberLit x)   = x
  show (CharLit x)     = show x
  show (StringLit x)   = show x


instance Show QVar where
  show (QVar x y) = showQualified x y

instance Show QCtor where
  show (QCtor x y) = showQualified x y

instance Show QVarOp where
  show (QVarOp x y) = showQualified x y

instance Show QCtorOp where
  show (QCtorOp x y) = showQualified x y

instance Show QClass where
  show (QClass x y) = showQualified x y


showQualified :: (Show a, Show b) => Maybe a -> b -> String
showQualified x y = foldMap ((++ ".") . show) x ++ show y
