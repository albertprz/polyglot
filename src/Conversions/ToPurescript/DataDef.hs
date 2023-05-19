module Conversions.ToPurescript.DataDef where

import qualified SyntaxTrees.Haskell.DataDef    as H
import qualified SyntaxTrees.Purescript.DataDef as P

import Conversions.ToPurescript.Common
import Conversions.ToPurescript.Type

typeDef :: H.TypeDef -> P.TypeDef
typeDef (H.TypeDef x y z) =
  P.TypeDef (typeVar x) (typeParam <$> y) (anyKindedType z)


-- TODO: Newtype Deriving to standalone deriving
newtypeDef :: H.NewTypeDef -> P.NewTypeDef
newtypeDef (H.NewTypeDef x y z t _) =
  P.NewTypeDef (typeVar x) (typeParam <$> y) (ctor z) (fieldDef t)


-- TODO: Data Deriving to standalone deriving
dataDef :: H.DataDef -> P.DataDef
dataDef (H.DataDef x y z _) =
  P.DataDef (typeVar x) (typeParam <$> y) (dataCtorDef <$> z)


dataCtorDef :: H.DataCtorDef -> P.DataCtorDef
dataCtorDef (H.UnNamedFieldsCtor x y) =
  P.UnNamedFieldsCtor (ctor x) (unnamedFieldDef <$> y)
dataCtorDef (H.NamedFieldsCtor x y) =
  P.NamedFieldsCtor (ctor x) (namedFieldDef <$> y)


fieldDef :: H.FieldDef -> P.FieldDef
fieldDef (H.UnNamedField x) = P.UnNamedField $ unnamedFieldDef x
fieldDef (H.NamedField x)   = P.NamedField $ namedFieldDef x

unnamedFieldDef :: H.UnNamedFieldDef -> P.UnNamedFieldDef
unnamedFieldDef (H.UnNamedFieldDef x) =
  P.UnNamedFieldDef (type' x)

namedFieldDef :: H.NamedFieldDef -> P.NamedFieldDef
namedFieldDef (H.NamedFieldDef x y) =
  P.NamedFieldDef (var x) (type' y)
