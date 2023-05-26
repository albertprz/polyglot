module SyntaxTrees.Purescript.Type where

import Data.List                     (intercalate)
import SyntaxTrees.Purescript.Common (Module, QClass, showQualified)
import Utils.String                  (str, wrapParens, wrapParensCsv,
                                      wrapSpaces, (+++))


newtype TypeParam
  = TypeParam String
  deriving (Eq, Ord)

newtype TypeVar
  = TypeVar String

data TypeCtor
  = TypeCtor String
  | Arrow
  | TupleType

data AnyKindedType
  = TypeValue Type
  | TypeFn QTypeCtor

data ClassConstraint
  = ClassConstraint QClass [Type]

data Type
  = CtorTypeApply QTypeCtor [Type]
  | ParamTypeApply TypeParam [Type]
  | NestedTypeApply Type [Type]
  | TypeVar' QTypeVar
  | TypeParam' TypeParam
  | TypeScope [TypeParam] Type
  | ClassScope [ClassConstraint] Type


data QTypeVar
  = QTypeVar (Maybe Module) TypeVar

data QTypeCtor
  = QTypeCtor (Maybe Module) TypeCtor



instance Show TypeParam where
  show (TypeParam x) = x

instance Show TypeVar where
  show (TypeVar x) = x

instance Show TypeCtor where
  show (TypeCtor x) = x
  show Arrow        = "->"
  show TupleType    = "()"

instance Show AnyKindedType where
  show (TypeValue x) = show x
  show (TypeFn x)    = show x

instance Show Type where
  show (CtorTypeApply (QTypeCtor _ Arrow) x)        = intercalate (wrapSpaces "->")
    (showArrowTypeNested <$> x)
  show (CtorTypeApply x@(QTypeCtor _ (TypeCtor _)) y) = show x +++
    (intercalate " " $ showTypeNested <$> y)
  show (CtorTypeApply (QTypeCtor _ TupleType) x)    = str (wrapSpaces "/\\") x
  show (ParamTypeApply x y) = show x  +++ (intercalate " " $ showTypeNested <$> y)
  show (NestedTypeApply x y) = show x +++ (intercalate " " $ showTypeNested <$> y)
  show (TypeVar' x) = show x
  show (TypeParam' x) = show x
  show (TypeScope x y) = "forall" +++ str " " x ++ "." +++ showTypeScopeNested y
  show (ClassScope x y) = str (wrapSpaces "=>") x +++ "=>" +++ showClassScopeNested y

instance Show ClassConstraint where
  show (ClassConstraint x [y]) = show x +++ show y
  show (ClassConstraint x y)   = show x +++ wrapParensCsv y

instance Show QTypeVar where
  show (QTypeVar x y) = showQualified x y

instance Show QTypeCtor where
  show (QTypeCtor x y) = showQualified x y


showAnyKindedTypeNested :: AnyKindedType -> String
showAnyKindedTypeNested (TypeValue x) = showTypeNested x
showAnyKindedTypeNested (TypeFn x)    = show x

showTypeNested :: Type -> String
showTypeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      CtorTypeApply (QTypeCtor _ (TypeCtor _)) _ -> True
      CtorTypeApply (QTypeCtor _ Arrow) _        -> True
      CtorTypeApply (QTypeCtor _ TupleType) _    -> True
      ParamTypeApply _ _                         -> True
      NestedTypeApply _ _                        -> True
      TypeScope _ _                              -> True
      ClassScope _ _                             -> True
      _                                          -> False

showArrowTypeNested :: Type -> String
showArrowTypeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      CtorTypeApply (QTypeCtor _ Arrow) _ -> True
      TypeScope _ _                       -> True
      ClassScope _ _                      -> True
      _                                   -> False

showTypeScopeNested :: Type -> String
showTypeScopeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      TypeScope _ _ -> True
      _             -> False

showClassScopeNested :: Type -> String
showClassScopeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      TypeScope _ _  -> True
      ClassScope _ _ -> True
      _              -> False
