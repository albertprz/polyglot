module SyntaxTrees.Scala.Common where

import Data.List (intercalate)


newtype Var
  = Var String

newtype Ctor
  = Ctor String


newtype VarOp
  = VarOp String

newtype CtorOp
  = CtorOp String

newtype TypeClass
  = TypeClass String

newtype Package
  = Package [String]


data Literal
  = UnitLit
  | BoolLit Bool
  | IntLit String
  | FloatLit String
  | CharLit Char
  | StringLit String

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


data QVar
  = QVar (Maybe Package) Var

data QCtor
  = QCtor (Maybe Package) Ctor

data QVarOp
  = QVarOp (Maybe Package) VarOp

data QCtorOp
  = QCtorOp (Maybe Package) CtorOp

data QTypeClass
  = QTypeClass (Maybe Package) TypeClass


instance Show Var where
  show (Var x) = x

instance Show Ctor where
  show (Ctor x) = x

instance Show VarOp where
  show (VarOp x) = x

instance Show CtorOp where
  show (CtorOp x) = x

instance Show TypeClass where
  show (TypeClass x) = x


instance Show Package where
  show (Package x) = intercalate "." x

instance Show Literal where
  show UnitLit         = "()"
  show (BoolLit True)  = "true"
  show (BoolLit False) = "false"
  show (IntLit x)      = x
  show (FloatLit x)    = x
  show (CharLit x)     = show x
  show (StringLit x)   = show x


instance Show Modifier where
  show Implicit  = "implicit"
  show Lazy      = "lazy"
  show Abstract  = "abstract"
  show Private   = "private"
  show Public    = "public"
  show Protected = "protected"
  show Override  = "override"
  show Sealed    = "sealed"
  show Open      = "open"


instance Show QVar where
  show (QVar x y) = showQualified x y

instance Show QCtor where
  show (QCtor x y) = showQualified x y

instance Show QVarOp where
  show (QVarOp x y) = showQualified x y

instance Show QCtorOp where
  show (QCtorOp x y) = showQualified x y

instance Show QTypeClass where
  show (QTypeClass x y) = showQualified x y


showQualified :: (Show a, Show b) => Maybe a -> b -> String
showQualified x y = foldMap ((++ ".") . show) x ++ show y
