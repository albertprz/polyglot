module Conversions.ToPurescript.Type where

import qualified SyntaxTrees.Haskell.Type    as H
import qualified SyntaxTrees.Purescript.Type as P

import Conversions.ToPurescript.Common (find, module', qClass)

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set



typeParam :: H.TypeParam -> P.TypeParam
typeParam (H.TypeParam x) = P.TypeParam  x

typeVar :: H.TypeVar -> P.TypeVar
typeVar (H.TypeVar x) = P.TypeVar $ convertTypeVar x
typeVar H.UnitType    = P.TypeVar "Unit"

typeCtor :: H.TypeCtor -> P.TypeCtor
typeCtor H.Arrow        = P.Arrow
typeCtor H.TupleType    = P.TupleType
typeCtor (H.TypeCtor x) = P.TypeCtor $ convertTypeCtor x
typeCtor H.ListType     = P.TypeCtor "Array"

anyKindedType :: H.AnyKindedType -> P.AnyKindedType
anyKindedType (H.TypeValue x) = P.TypeValue $ type' x
anyKindedType (H.TypeFn x)    = P.TypeFn $ qTypeCtor x


classConstraint :: H.ClassConstraint -> P.ClassConstraint
classConstraint (H.ClassConstraint x y) =
  P.ClassConstraint (qClass x) (type' <$> y)


type' :: H.Type -> P.Type
type' (H.CtorTypeApply x y)   = P.CtorTypeApply (qTypeCtor x)
                                                (type' <$> y)
type' (H.ParamTypeApply x y)  = P.ParamTypeApply (typeParam x)
                                                 (type' <$> y)
type' (H.NestedTypeApply x y) = P.NestedTypeApply (type' x) (type' <$> y)
type' (H.TypeVar' x)          = P.TypeVar' $ qTypeVar x
type' (H.TypeParam' x)        = P.TypeParam' $ typeParam x
type' (H.TypeScope x y)       = P.TypeScope (typeParam <$> x) (type' y)
type' (H.ClassScope x y)      = P.ClassScope (classConstraint <$> x)
                                             (type' y)


findTypeParams :: H.Type -> Set H.TypeParam
findTypeParams (H.CtorTypeApply _ y)   = mconcat $ findTypeParams <$> y
findTypeParams (H.ParamTypeApply x y)  = (Set.singleton x) <>
                                         (mconcat $ findTypeParams <$> y)
findTypeParams (H.NestedTypeApply x y) = mconcat $ findTypeParams <$>
                                         (x : y)
findTypeParams (H.TypeVar' _)          = Set.empty
findTypeParams (H.TypeParam' x)        = Set.singleton x
findTypeParams (H.TypeScope _ _)       = Set.empty
findTypeParams (H.ClassScope _ y)      = findTypeParams y


extractTypes :: H.Type -> [H.Type]
extractTypes (H.CtorTypeApply (H.QTypeCtor _ H.Arrow) y) = y
extractTypes (H.TypeScope _ y)                           = extractTypes y
extractTypes (H.ClassScope _ y)                          = extractTypes y
extractTypes _                                           = []


qTypeVar :: H.QTypeVar -> P.QTypeVar
qTypeVar (H.QTypeVar x y) = P.QTypeVar (module' <$> x) (typeVar y)

qTypeCtor :: H.QTypeCtor -> P.QTypeCtor
qTypeCtor (H.QTypeCtor x y) = P.QTypeCtor (module' <$> x) (typeCtor y)




convertTypeCtor :: String -> String
convertTypeCtor x = find typeCtorMap x

convertTypeVar :: String -> String
convertTypeVar x = find typeVarMap x

typeCtorMap :: Map String String
typeCtorMap = Map.empty

typeVarMap :: Map String String
typeVarMap = Map.empty
