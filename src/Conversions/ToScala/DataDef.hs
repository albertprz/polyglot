module Conversions.ToScala.DataDef where

import ClassyPrelude

import qualified SyntaxTrees.Haskell.DataDef as H
import qualified SyntaxTrees.Scala.Common    as S
import qualified SyntaxTrees.Scala.DataDef   as S
import qualified SyntaxTrees.Scala.Type      as S

import Conversions.ToScala.Common  (autoIds, class', ctor, var)
import Conversions.ToScala.Type    (anyKindedType, type', typeCtor, typeParam)
import SyntaxTrees.Haskell.DataDef (derivingClasses)


typeDef :: H.TypeDef -> S.TypeDef
typeDef (H.TypeDef x y z) =
  S.TypeDef (typeCtor x) (typeParam <$> y) (anyKindedType z)


newtypeDef :: H.NewTypeDef -> S.OpaqueTypeDef
newtypeDef (H.NewTypeDef x y _  z t) =
  S.OpaqueTypeDef (typeCtor x) (typeParam <$> y)
                  ((.type') $ fieldDef z)
                  (class' <$> foldMap ((snd <$>) . derivingClasses) t)


dataDef :: H.DataDef -> S.EnumDef
dataDef (H.DataDef x y z t) =
  S.EnumDef [] (typeCtor x) (typeParam <$> y) [] []
               (class' <$> foldMap ((snd <$>) . derivingClasses) t)
               (dataCtorDef <$> z)


dataCtorDef :: H.DataCtorDef -> S.EnumCaseDef
dataCtorDef (H.NamedFieldsCtor x y) = S.EnumCaseDef (ctor x)
        [S.ArgList $ namedFieldDef <$> y] [] [] []
dataCtorDef (H.UnNamedFieldsCtor x y) = S.EnumCaseDef (ctor x)
        [S.ArgList $ uncurry unNamedFieldDef <$> zip autoIds y] [] [] []


fieldDef :: H.FieldDef -> S.ArgField
fieldDef (H.NamedField x)   = namedFieldDef x
fieldDef (H.UnNamedField x) = unNamedFieldDef "value" x


namedFieldDef :: H.NamedFieldDef -> S.ArgField
namedFieldDef (H.NamedFieldDef x y) = S.ArgField [] (var x) (type' y)

unNamedFieldDef :: Text -> H.UnNamedFieldDef -> S.ArgField
unNamedFieldDef x (H.UnNamedFieldDef y) = S.ArgField [] (S.Var x) (type' y)
