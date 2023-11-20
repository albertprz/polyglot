module SyntaxTrees.Haskell.Type where

import Data.Text                  (Text)
import SyntaxTrees.Haskell.Common (Module, QClass)


newtype TypeParam
  = TypeParam Text
  deriving (Eq, Ord)

data TypeVar
  = TypeVar Text
  | UnitType

data TypeCtor
  = TypeCtor Text
  | Arrow
  | TupleType
  | ListType

data AnyKindedType
  = TypeValue Type
  | TypeFn QTypeCtor

data ClassConstraint
  = ClassConstraint QClass [Type]

data Type
  = CtorTypeApply QTypeCtor [Type]
  | ParamTypeApply TypeParam [Type]
  | NestedTypeApply Type [Type]
  | TypeVar' QTypeVar
  | TypeParam' TypeParam
  | TypeScope [TypeParam] Type
  | ClassScope [ClassConstraint] Type


data QTypeVar
  = QTypeVar (Maybe Module) TypeVar

data QTypeCtor
  = QTypeCtor (Maybe Module) TypeCtor
