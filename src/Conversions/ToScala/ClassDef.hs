module Conversions.ToScala.ClassDef where

import ClassyPrelude

import qualified SyntaxTrees.Haskell.ClassDef as H
import qualified SyntaxTrees.Haskell.Common   as H
import qualified SyntaxTrees.Scala.Common     as S
import qualified SyntaxTrees.Scala.DataDef    as S
import qualified SyntaxTrees.Scala.FnDef      as S
import qualified SyntaxTrees.Scala.Type       as S

import Conversions.ToScala.FnDef (fnDefOrSigs, fnDefs)
import Conversions.ToScala.Type  (anyKindedType, classConstraint,
                                  findAnyKindedTypeParams, typeParam,
                                  usingArgList)




classDef :: H.ClassDef -> S.TraitDef
classDef (H.ClassDef x (H.Class y) z t) =
  S.TraitDef [] (S.TypeCtor y)
        (typeParam <$> z) [] [usingArgList $ classConstraint <$> x] []
        (S.Fn . fnDefs <$> fnDefOrSigs t)


instanceDef :: H.InstanceDef -> S.GivenDef
instanceDef (H.InstanceDef x (H.Class y) z t) =
  S.GivenDef [] Nothing
        (mconcat ((typeParam <$>) . toList . findAnyKindedTypeParams <$> z))
        [usingArgList $ classConstraint <$> x]
        (S.CtorTypeApply (S.QTypeCtor Nothing $ S.TypeCtor y) $ anyKindedType <$> z)
        (Right $ fnDefs <$> fnDefOrSigs t)

derivingDef :: H.DerivingDef -> S.GivenDef
derivingDef (H.DerivingDef _ x (H.Class y) z _) =
  S.GivenDef [] Nothing
        (mconcat ((typeParam <$>) . toList . findAnyKindedTypeParams <$> z))
        [usingArgList $ classConstraint <$> x]
        (S.CtorTypeApply (S.QTypeCtor Nothing $ S.TypeCtor y) $ anyKindedType <$> z)
        (Left $ S.FnVar' $ S.Var' (S.QVar Nothing $ S.Var "derived"))
