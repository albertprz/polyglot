module Conversions.HaskellToScala.ClassDef where

import qualified SyntaxTrees.Haskell.ClassDef as H
import qualified SyntaxTrees.Haskell.Common   as H
import qualified SyntaxTrees.Haskell.FnDef    as H
import qualified SyntaxTrees.Scala.DataDef    as S
import qualified SyntaxTrees.Scala.FnDef      as S
import qualified SyntaxTrees.Scala.Type       as S

import Conversions.HaskellToScala.FnDef (fnDe, fnDefOrSigs)
import Conversions.HaskellToScala.Type
import Data.Foldable                    (Foldable (toList))
import Data.List                        (nubBy)
import Data.Maybe                       (mapMaybe)
import Utils.List                       (groupTuplesByKey, mergeUnion)



classDef :: H.ClassDef -> S.TraitDef
classDef (H.ClassDef x (H.Class y) z t) = S.TraitDef [] (S.TypeVar y)
        (typeParam <$> z) [] [usingArgList $ classConstraint <$> x] []
        ((S.Method . fnDefs) <$> fnDefOrSigs t)


instanceDef :: H.InstanceDef -> S.GivenDef
instanceDef (H.InstanceDef x (H.Class y) z t) = S.GivenDef [] Nothing
        (mconcat ((typeParam <$>) . toList . findAnyKindedTypeParams <$> z))
        [usingArgList $ classConstraint <$> x]
        (S.CtorTypeApply (S.TypeCtor y) $ anyKindedType <$> z)
        (fnDefs <$> fnDefOrSigs t)
