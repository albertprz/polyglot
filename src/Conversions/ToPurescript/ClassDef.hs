module Conversions.ToPurescript.ClassDef where

import ClassyPrelude

import qualified SyntaxTrees.Haskell.ClassDef    as H
import qualified SyntaxTrees.Purescript.ClassDef as P

import Conversions.ToPurescript.Common  (class')
import Conversions.ToPurescript.DataDef (derivingStrategy)
import Conversions.ToPurescript.FnDef   (fnDefOrSig)
import Conversions.ToPurescript.Type    (anyKindedType, classConstraint,
                                         typeParam)


classDef :: H.ClassDef -> P.ClassDef
classDef (H.ClassDef x y z t) =
  P.ClassDef (classConstraint <$> x) (class' y)
             (typeParam <$> z) (fnDefOrSig <$> t)


instanceDef :: H.InstanceDef -> P.InstanceDef
instanceDef (H.InstanceDef x y z t) =
  P.InstanceDef (classConstraint <$> x) Nothing (class' y)
                (anyKindedType <$> z) (fnDefOrSig <$> t)

derivingDef :: H.DerivingDef -> P.DerivingDef
derivingDef (H.DerivingDef x y z t _) =
  P.DerivingDef (derivingStrategy x) (classConstraint <$> y)
                Nothing (class' z) (anyKindedType <$> t)
