module SyntaxTrees.Haskell.Type where

import SyntaxTrees.Haskell.Common (Class, Module, QClass)


newtype TypeParam
  = TypeParam String
  deriving (Eq, Ord, Show)

data TypeVar
  = TypeVar String
  | UnitType
  deriving (Show)

data TypeCtor
  = TypeCtor String
  | Arrow
  | TupleType
  | ListType
  deriving (Show)

data AnyKindedType
  = TypeValue Type
  | TypeFn QTypeCtor
  deriving (Show)

data ClassConstraint
  = ClassConstraint QClass [Type]
  deriving (Show)

data Type
  = CtorTypeApply QTypeCtor [Type]
  | ParamTypeApply TypeParam [Type]
  | NestedTypeApply Type [Type]
  | TypeVar' QTypeVar
  | TypeParam' TypeParam
  | TypeScope [TypeParam] Type
  | ClassScope [ClassConstraint] Type
  deriving (Show)


data QTypeVar
  = QTypeVar (Maybe Module) TypeVar
  deriving (Show)

data QTypeCtor
  = QTypeCtor (Maybe Module) TypeCtor
  deriving (Show)
