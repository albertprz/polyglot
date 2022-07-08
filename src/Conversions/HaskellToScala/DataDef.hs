module Conversions.HaskellToScala.DataDef where

import qualified SyntaxTrees.Haskell.DataDef as H
import qualified SyntaxTrees.Scala.Common    as S
import qualified SyntaxTrees.Scala.DataDef   as S
import qualified SyntaxTrees.Scala.Type      as S

import Conversions.HaskellToScala.Common
import Conversions.HaskellToScala.Type


typeDef :: H.TypeDef -> S.TypeDef
typeDef (H.TypeDef x y z) =
  S.TypeDef (typeVar x) (typeParam <$> y) (anyKindedType z)

newtypeDef :: H.NewTypeDef -> S.OpaqueTypeDef
newtypeDef (H.NewTypeDef x y _  z t) =
  S.OpaqueTypeDef (typeVar x) (typeParam <$> y) (fieldDef z) (class' <$> t) []

dataDef :: H.DataDef -> S.EnumDef
dataDef (H.DataDef x y z t) =
  S.EnumDef [] (typeVar x) (typeParam <$> y) [] [] (class' <$> t)
               (dataCtorDef <$> z)


dataCtorDef :: H.DataCtorDef -> S.EnumCaseDef
dataCtorDef (H.NamedFieldsCtor x y) = S.EnumCaseDef (ctor x)
        [S.ArgList $ namedFieldDef <$> y] [] [] []
dataCtorDef (H.UnNamedFieldsCtor x y) = S.EnumCaseDef (ctor x)
        [S.ArgList $ uncurry unNamedFieldDef <$> zip autoIds y] [] [] []
  where
    autoIds = ("value" ++) . show <$> nums

    nums :: [Integer]
    nums = iterate (+ 1) 1


fieldDef :: H.FieldDef -> S.ArgField
fieldDef (H.NamedField x)   = namedFieldDef x
fieldDef (H.UnNamedField x) = unNamedFieldDef "value" x


namedFieldDef :: H.NamedFieldDef -> S.ArgField
namedFieldDef (H.NamedFieldDef x y) = S.ArgField [] (var x) (type' y)

unNamedFieldDef :: String -> H.UnNamedFieldDef -> S.ArgField
unNamedFieldDef x (H.UnNamedFieldDef y) = S.ArgField [] (S.Var x) (type' y)
