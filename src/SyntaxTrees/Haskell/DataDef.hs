module SyntaxTrees.Haskell.DataDef  where

import SyntaxTrees.Haskell.Common ( Term, Type )

data TypeDef = TypeDef {
    alias :: Type
  , type' :: Type
}

data NewTypeDef = NewTypeDef {
    type' :: Type
  , ctor  :: Term
  , field :: FieldDef
}

data DataDef = DataDef {
    type'    :: Type
  , ctorDefs :: [DataCtorDef]
}

data DataCtorDef =
    UnNamedFieldsCtor {
    ctor          :: Term
  , unnamedFields :: [UnNamedFieldDef]
} | NamedFieldsCtor {
    ctor        :: Term
  , namedFields :: [NamedFieldDef]
}

newtype UnNamedFieldDef = UnNamedFieldDef {
    type' :: Type
}

data NamedFieldDef = NamedFieldDef {
    name  :: Term
  , type' :: Type
}

data FieldDef = UnNamedField UnNamedFieldDef |
                NamedField   NamedFieldDef
