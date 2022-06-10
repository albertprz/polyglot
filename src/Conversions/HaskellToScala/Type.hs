module Conversions.HaskellToScala.Type where

import qualified SyntaxTrees.Haskell.Type as H
import qualified SyntaxTrees.Scala.Type   as S

import Conversions.HaskellToScala.Common

import Data.Char (toUpper)
import Data.Set  (Set)

import qualified Data.Set                 as Set
import           SyntaxTrees.Haskell.Type (TypeParam)


typeParam :: H.TypeParam -> S.TypeParam
typeParam (H.TypeParam x) = S.TypeParam $ toUpper <$> x

typeVar :: H.TypeVar -> S.TypeVar
typeVar (H.TypeVar x) = S.TypeVar x
typeVar H.UnitType    = S.TypeVar "Unit"

typeCtor :: H.TypeCtor -> S.TypeCtor
typeCtor H.Arrow        = S.Arrow
typeCtor H.TupleType    = S.TupleType
typeCtor (H.TypeCtor x) = S.TypeCtor x
typeCtor H.ListType     = S.TypeCtor "List"

anyKindedType :: H.AnyKindedType -> S.AnyKindedType
anyKindedType (H.TypeValue x) = S.TypeValue $ type' x
anyKindedType (H.TypeFn x)    = S.TypeFn $ typeCtor x

classConstraint :: H.ClassConstraint -> S.ClassConstraint
classConstraint (H.ClassConstraint x y) = S.ClassConstraint (class' x) (type' y)


type' :: H.Type -> S.Type
type' (H.CtorTypeApply x y)   = S.CtorTypeApply (typeCtor x) (type' <$> y)
type' (H.ParamTypeApply x y)  = S.ParamTypeApply (typeParam x) (type' <$> y)
type' (H.NestedTypeApply x y) = S.NestedTypeApply (type' x) (type' <$> y)
type' (H.TypeVar' x)          = S.TypeVar' $ typeVar x
type' (H.TypeParam' x)        = S.TypeParam' $ typeParam x
type' (H.TypeScope x y)       = S.TypeScope (typeParam <$> x) (type' y)
type' (H.ClassScope x y)      = S.ClassScope (classConstraint <$> x) (type' y)


typeSplit :: Int -> H.Type -> ([S.Type], S.Type)
typeSplit n tpe = (args, S.CtorTypeApply S.Arrow ret)
  where
    (args, ret) = splitAt n $ type' <$> extractTypes tpe

findTypeParams :: H.Type -> Set TypeParam
findTypeParams (H.CtorTypeApply _ y)   = mconcat $ findTypeParams <$> y
findTypeParams (H.ParamTypeApply x y)  = (Set.singleton x) <>
                                         (mconcat $ findTypeParams <$> y)
findTypeParams (H.NestedTypeApply x y) = mconcat $ findTypeParams <$> (x : y)
findTypeParams (H.TypeVar' _)          = Set.empty
findTypeParams (H.TypeParam' x)        = Set.singleton x
findTypeParams (H.TypeScope x y)       = Set.empty
findTypeParams (H.ClassScope _ y)      = findTypeParams y

extractTypes :: H.Type -> [H.Type]
extractTypes (H.CtorTypeApply H.Arrow y) = y
extractTypes (H.TypeScope _ y)           = extractTypes y
extractTypes (H.ClassScope _ y)          = extractTypes y
extractTypes _                           = []
