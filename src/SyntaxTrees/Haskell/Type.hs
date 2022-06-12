module SyntaxTrees.Haskell.Type where

import SyntaxTrees.Haskell.Common (Class)


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
  | TypeFn TypeCtor
  deriving (Show)

data ClassConstraint
  = ClassConstraint Class [Type]
  deriving (Show)

data Type
  = CtorTypeApply TypeCtor [Type]
  | ParamTypeApply TypeParam [Type]
  | NestedTypeApply Type [Type]
  | TypeVar' TypeVar
  | TypeParam' TypeParam
  | TypeScope [TypeParam] Type
  | ClassScope [ClassConstraint] Type
  deriving (Show)
