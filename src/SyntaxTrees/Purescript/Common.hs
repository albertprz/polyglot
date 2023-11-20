module SyntaxTrees.Purescript.Common where

import Data.List    (intercalate)
import Data.Text    (Text, unpack)
import Utils.String (wrapParens)


newtype Var
  = Var Text

newtype Ctor
  = Ctor Text

newtype VarOp
  = VarOp Text

newtype CtorOp
  = CtorOp Text

newtype Class
  = Class Text

newtype Module
  = Module [Text]

data Literal
  = UnitLit
  | BoolLit Bool
  | IntLit Text
  | NumberLit Text
  | CharLit Char
  | StringLit Text


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
  show (Var x) = unpack x

instance Show Ctor where
  show (Ctor x) = unpack x

instance Show VarOp where
  show (VarOp x) = unpack x

instance Show CtorOp where
  show (CtorOp x) = unpack x

instance Show Class where
  show (Class x) = unpack x

instance Show Module where
  show (Module x) = intercalate "." $ fmap unpack x

instance Show Literal where
  show UnitLit                             = "unit"
  show (BoolLit True)                      = "true"
  show (BoolLit False)                     = "false"
  show (IntLit (unpack -> x@('-' : _)))    = wrapParens x
  show (IntLit x)                          = unpack x
  show (NumberLit (unpack -> x@('-' : _))) = wrapParens x
  show (NumberLit x)                       = unpack x
  show (CharLit x)                         = show x
  show (StringLit x)                       = show $ unpack x


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
showQualified x y = foldMap ((<> ".") . show) x <> show y
