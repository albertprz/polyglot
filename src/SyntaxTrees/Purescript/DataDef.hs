module SyntaxTrees.Purescript.DataDef where

import SyntaxTrees.Purescript.Common (Ctor, Var)
import SyntaxTrees.Purescript.Type   (AnyKindedType, Type, TypeParam, TypeVar)


data TypeDef
  = TypeDef
      { alias      :: TypeVar
      , typeParams :: [TypeParam]
      , type'      :: AnyKindedType
      }
  deriving (Show)

data NewTypeDef
  = NewTypeDef
      { type'      :: TypeVar
      , typeParams :: [TypeParam]
      , ctor       :: Ctor
      , field      :: FieldDef
      }
  deriving (Show)

data DataDef
  = DataDef
      { type'      :: TypeVar
      , typeParams :: [TypeParam]
      , ctorDefs   :: [DataCtorDef]
      }
  deriving (Show)

data DataCtorDef
  = UnNamedFieldsCtor
      { ctor          :: Ctor
      , unnamedFields :: [UnNamedFieldDef]
      }
  | NamedFieldsCtor
      { ctor        :: Ctor
      , namedFields :: [NamedFieldDef]
      }
  deriving (Show)

data FieldDef
  = UnNamedField UnNamedFieldDef
  | NamedField NamedFieldDef
  deriving (Show)

data UnNamedFieldDef
  = UnNamedFieldDef
      { type' :: Type
      }
  deriving (Show)

data NamedFieldDef
  = NamedFieldDef
      { name  :: Var
      , type' :: Type
      }
  deriving (Show)
