module SyntaxTrees.Scala.Type where

import SyntaxTrees.Scala.Common (Modifier, TypeClass, Var)


newtype TypeParam
  = TypeParam String
  deriving (Show)

newtype TypeVar
  = TypeVar String
  deriving (Show)


newtype ArgList
  = ArgList [ArgField]
  deriving (Show)

newtype UsingArgList
  = UsingArgList [UsingArgField]
  deriving (Show)


data ArgField
  = ArgField
      { modifiers :: [Modifier]
      , name      :: Var
      , type'     :: Type
      }
  deriving (Show)

data UsingArgField
  = UsingArgField
      { modifiers :: [Modifier]
      , name      :: Maybe Var
      , type'     :: Type
      }
  deriving (Show)


data TypeCtor
  = TypeCtor String
  | Arrow
  | TupleType
  deriving (Show)

data AnyKindedType
  = TypeValue Type
  | TypeFn TypeCtor
  deriving (Show)

data ClassConstraint
  = ClassConstraint TypeClass Type
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
