module SyntaxTrees.Haskell.DataDef where

import SyntaxTrees.Haskell.Common (Class, Ctor, Var)
import SyntaxTrees.Haskell.Type   (AnyKindedType, Type, TypeCtor, TypeParam)


data TypeDef
  = TypeDef
      { alias      :: TypeCtor
      , typeParams :: [TypeParam]
      , type'      :: AnyKindedType
      }
  deriving (Show)

data NewTypeDef
  = NewTypeDef
      { type'      :: TypeCtor
      , typeParams :: [TypeParam]
      , ctor       :: Ctor
      , field      :: FieldDef
      , deriving'  :: [DerivingClause]
      }
  deriving (Show)

data DataDef
  = DataDef
      { type'      :: TypeCtor
      , typeParams :: [TypeParam]
      , ctorDefs   :: [DataCtorDef]
      , deriving'  :: [DerivingClause]
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

data DerivingClause
  = Deriving DerivingStrategy [Class]
  | DerivingVia [Class] AnyKindedType
  deriving (Show)

data DerivingStrategy
  = StandardDeriving
  | NewTypeDeriving
  | AnyClassDeriving
  deriving (Eq, Show)


derivingClasses :: DerivingClause -> [(DerivingStrategy, Class)]
derivingClasses (Deriving x y)    = (x,) <$> y
derivingClasses (DerivingVia x _) = (StandardDeriving,) <$> x
