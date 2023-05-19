module Conversions.ToPurescript.ClassDef where

import qualified SyntaxTrees.Haskell.ClassDef    as H
import qualified SyntaxTrees.Purescript.ClassDef as P

import Conversions.ToPurescript.Common
import Conversions.ToPurescript.FnDef
import Conversions.ToPurescript.Type


classDef :: H.ClassDef -> P.ClassDef
classDef (H.ClassDef x y z t) =
  P.ClassDef (classConstraint <$> x) (class' y)
             (typeParam <$> z) (fnDefOrSig <$> t)


instanceDef :: H.InstanceDef -> P.InstanceDef
instanceDef (H.InstanceDef x y z t) =
  P.InstanceDef (classConstraint <$> x) Nothing (class' y)
                (anyKindedType <$> z) (fnDefOrSig <$> t)

derivingDef :: H.DerivingDef -> P.DerivingDef
derivingDef (H.DerivingDef _ x y z t) =
  P.DerivingDef (classConstraint <$> x) Nothing (class' y)
                (anyKindedType <$> z) (class' <$> t)
