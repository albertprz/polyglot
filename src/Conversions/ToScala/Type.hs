module Conversions.ToScala.Type where

import qualified SyntaxTrees.Haskell.Type as H
import qualified SyntaxTrees.Scala.Common as S
import qualified SyntaxTrees.Scala.Type   as S

import Conversions.ToScala.Common (find, qClass, qualifier')

import Data.Char (toUpper)
import Data.Map  (Map)
import Data.Set  (Set)

import qualified Data.Map as Map
import qualified Data.Set as Set



typeParam :: H.TypeParam -> S.TypeParam
typeParam (H.TypeParam x) = S.TypeParam $ toUpper <$> x

typeVar :: H.TypeVar -> S.TypeVar
typeVar (H.TypeVar x) = S.TypeVar $ convertTypeVar x
typeVar H.UnitType    = S.TypeVar "Unit"

typeCtor :: H.TypeCtor -> S.TypeCtor
typeCtor H.Arrow        = S.Arrow
typeCtor H.TupleType    = S.TupleType
typeCtor (H.TypeCtor x) = S.TypeCtor $ convertTypeCtor x
typeCtor H.ListType     = S.TypeCtor "List"

anyKindedType :: H.AnyKindedType -> S.Type
anyKindedType (H.TypeValue x) = type' x
anyKindedType (H.TypeFn (H.QTypeCtor _ H.ListType)) =
  S.CtorTypeApply (S.QTypeCtor Nothing $ S.TypeCtor "List") [S.ExistentialType]
anyKindedType (H.TypeFn (H.QTypeCtor _ H.Arrow)) =
  S.CtorTypeApply (S.QTypeCtor Nothing $ S.TypeCtor "Function2")
                                             [S.ExistentialType, S.ExistentialType]
anyKindedType (H.TypeFn (H.QTypeCtor _ H.TupleType)) =
  S.CtorTypeApply (S.QTypeCtor Nothing $ S.TypeCtor "Tuple2")
                                             [S.ExistentialType, S.ExistentialType]
anyKindedType (H.TypeFn x@(H.QTypeCtor _ _)) = S.CtorTypeApply (qTypeCtor x)
                                             [S.ExistentialType]


classConstraint :: H.ClassConstraint -> S.ClassConstraint
classConstraint (H.ClassConstraint x y) = S.ClassConstraint (qClass x) (type' <$> y)


type' :: H.Type -> S.Type
type' (H.CtorTypeApply x y)   = S.CtorTypeApply (qTypeCtor x) (type' <$> y)
type' (H.ParamTypeApply x y)  = S.ParamTypeApply (typeParam x) (type' <$> y)
type' (H.NestedTypeApply x y) = S.NestedTypeApply (type' x) (type' <$> y)
type' (H.TypeVar' x)          = S.TypeVar' $ qTypeVar x
type' (H.TypeParam' x)        = S.TypeParam' $ typeParam x
type' (H.TypeScope x y)       = S.TypeScope (typeParam <$> x) (type' y)
type' (H.ClassScope x y)      = S.ClassScope (classConstraint <$> x) (type' y)


typeSplit :: Int -> H.Type -> ([S.Type], S.Type)
typeSplit 0 tpe = ([], type' tpe)
typeSplit n tpe = (args, S.CtorTypeApply (S.QTypeCtor Nothing S.Arrow) ret)
  where
    (args, ret) = splitAt (min n (length types - 1)) (type' <$> types)
    types = extractTypes tpe


classScopeSplit :: H.Type -> ([S.ClassConstraint], H.Type)
classScopeSplit (H.ClassScope x y) = (classConstraint <$> x, y)
classScopeSplit x                  = ([], x)


findAnyKindedTypeParams :: H.AnyKindedType -> Set H.TypeParam
findAnyKindedTypeParams (H.TypeValue x) = findTypeParams x
findAnyKindedTypeParams _               = Set.empty


findTypeParams :: H.Type -> Set H.TypeParam
findTypeParams (H.CtorTypeApply _ y)   = mconcat $ findTypeParams <$> y
findTypeParams (H.ParamTypeApply x y)  = (Set.singleton x) <>
                                         (mconcat $ findTypeParams <$> y)
findTypeParams (H.NestedTypeApply x y) = mconcat $ findTypeParams <$> (x : y)
findTypeParams (H.TypeVar' _)          = Set.empty
findTypeParams (H.TypeParam' x)        = Set.singleton x
findTypeParams (H.TypeScope _ _)       = Set.empty
findTypeParams (H.ClassScope _ y)      = findTypeParams y


extractTypes :: H.Type -> [H.Type]
extractTypes (H.CtorTypeApply (H.QTypeCtor _ H.Arrow) y) = y
extractTypes (H.TypeScope _ y)                           = extractTypes y
extractTypes (H.ClassScope _ y)                          = extractTypes y
extractTypes _                                           = []


argLists :: [S.Var] -> [S.Type] -> [S.ArgList]
argLists args argTypes =
  S.ArgList . pure . uncurry (S.ArgField []) <$> zip args argTypes


usingArgList :: [S.ClassConstraint] -> S.UsingArgList
usingArgList constraints =
  S.UsingArgList $ (S.UsingArgField [] Nothing) <$> constraints


qTypeVar :: H.QTypeVar -> S.QTypeVar
qTypeVar (H.QTypeVar x y) = S.QTypeVar (qualifier' <$> x) (typeVar y)

qTypeCtor :: H.QTypeCtor -> S.QTypeCtor
qTypeCtor (H.QTypeCtor x y) = S.QTypeCtor (qualifier' <$> x) (typeCtor y)




convertTypeCtor :: String -> String
convertTypeCtor x =  find typeCtorMap x

convertTypeVar :: String -> String
convertTypeVar x =  find typeVarMap x

typeCtorMap :: Map String String
typeCtorMap = Map.fromList [("Maybe", "Option")]

typeVarMap :: Map String String
typeVarMap = Map.fromList [("Type", "Typex")]
