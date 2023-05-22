module Conversions.ToPurescript.DataDef where

import qualified SyntaxTrees.Haskell.DataDef     as H
import qualified SyntaxTrees.Haskell.Type        as H
import qualified SyntaxTrees.Purescript.ClassDef as P
import qualified SyntaxTrees.Purescript.DataDef  as P
import qualified SyntaxTrees.Purescript.Type     as P

import Conversions.ToPurescript.Common (class', ctor, var)
import Conversions.ToPurescript.Type   (anyKindedType, type', typeCtor,
                                        typeParam)
import SyntaxTrees.Haskell.DataDef     (derivingClasses)


typeDef :: H.TypeDef -> P.TypeDef
typeDef (H.TypeDef x y z) =
  P.TypeDef (typeCtor x) (typeParam <$> y) (anyKindedType z)


newtypeDef :: H.NewTypeDef -> (P.NewTypeDef, [P.DerivingDef])
newtypeDef (H.NewTypeDef x y z t u) =
  (newtype', extractDerivingDefs x y u)
  where
    newtype' = P.NewTypeDef (typeCtor x) (typeParam <$> y)
                            (ctor z) (fieldDef t)


dataDef :: H.DataDef -> (P.DataDef, [P.DerivingDef])
dataDef (H.DataDef x y z u) =
  (data', extractDerivingDefs x y u)
  where
    data' = P.DataDef (typeCtor  x) (typeParam <$> y)
                      (dataCtorDef <$> z)


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


extractDerivingDefs :: H.TypeCtor -> [H.TypeParam] -> [H.DerivingClause]
                    -> [P.DerivingDef]
extractDerivingDefs x y z =
  buildDeriving <$> foldMap derivingClasses z
  where
    buildDeriving (strat , cls) =
      P.DerivingDef (derivingStrategy strat) [] Nothing (class' cls) [classType]
    classType = case y of
      [] -> P.TypeValue $ P.TypeVar' $ P.QTypeVar
             (Nothing) (typeCtorToVar $ typeCtor x)
      _ -> P.TypeValue $ P.CtorTypeApply
                (P.QTypeCtor Nothing $ typeCtor x)
                ((P.TypeParam' . typeParam <$> y))

    typeCtorToVar (P.TypeCtor name) = P.TypeVar name
    typeCtorToVar _                 = P.TypeVar mempty


derivingStrategy :: H.DerivingStrategy -> P.DerivingStrategy
derivingStrategy H.NewTypeDeriving = P.NewTypeDeriving
derivingStrategy _                 = P.StandardDeriving
