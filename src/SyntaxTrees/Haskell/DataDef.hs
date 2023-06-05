module SyntaxTrees.Haskell.DataDef where

import SyntaxTrees.Haskell.Common (Class, Ctor, Var)
import SyntaxTrees.Haskell.Type   (AnyKindedType, Type, TypeCtor, TypeParam)


data TypeDef
  = TypeDef
      { alias      :: TypeCtor
      , typeParams :: [TypeParam]
      , type'      :: AnyKindedType
      }

data NewTypeDef
  = NewTypeDef
      { type'      :: TypeCtor
      , typeParams :: [TypeParam]
      , ctor       :: Ctor
      , field      :: FieldDef
      , deriving'  :: [DerivingClause]
      }

data DataDef
  = DataDef
      { type'      :: TypeCtor
      , typeParams :: [TypeParam]
      , ctorDefs   :: [DataCtorDef]
      , deriving'  :: [DerivingClause]
      }

data DataCtorDef
  = UnNamedFieldsCtor
      { ctor          :: Ctor
      , unnamedFields :: [UnNamedFieldDef]
      }
  | NamedFieldsCtor
      { ctor        :: Ctor
      , namedFields :: [NamedFieldDef]
      }

data FieldDef
  = UnNamedField UnNamedFieldDef
  | NamedField NamedFieldDef

data UnNamedFieldDef
  = UnNamedFieldDef
      { type' :: Type
      }

data NamedFieldDef
  = NamedFieldDef
      { name  :: Var
      , type' :: Type
      }

data DerivingClause
  = Deriving DerivingStrategy [Class]
  | DerivingVia [Class] AnyKindedType

data DerivingStrategy
  = StandardDeriving
  | NewTypeDeriving
  | AnyClassDeriving
  deriving (Eq)


derivingClasses :: DerivingClause -> [(DerivingStrategy, Class)]
derivingClasses (Deriving x y)    = (x,) <$> y
derivingClasses (DerivingVia x _) = (StandardDeriving,) <$> x
