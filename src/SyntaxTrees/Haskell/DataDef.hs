module SyntaxTrees.Haskell.DataDef  where

import SyntaxTrees.Haskell.Common ( Var, Type, TypeVar, AnyKindedType, Ctor )



data TypeDef = TypeDef {
    alias :: TypeVar
  , type' :: AnyKindedType
}

data NewTypeDef = NewTypeDef {
    type' :: TypeVar
  , ctor  :: Ctor
  , field :: FieldDef
}

data DataDef = DataDef {
    type'    :: TypeVar
  , ctorDefs :: [DataCtorDef]
}

data DataCtorDef =
    UnNamedFieldsCtor {
    ctor          :: Ctor
  , unnamedFields :: [UnNamedFieldDef]
} | NamedFieldsCtor {
    ctor          :: Ctor
  , namedFields   :: [NamedFieldDef]
}

newtype UnNamedFieldDef = UnNamedFieldDef {
    type' :: Type
}

data NamedFieldDef = NamedFieldDef {
    name  :: Var
  , type' :: Type
}

data FieldDef = UnNamedField UnNamedFieldDef |
                NamedField   NamedFieldDef
