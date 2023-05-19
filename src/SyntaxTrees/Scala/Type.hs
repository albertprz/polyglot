module SyntaxTrees.Scala.Type where

import Data.List                (intercalate)
import SyntaxTrees.Scala.Common (Modifier, Package, QTypeClass, Var,
                                 showQualified)
import Utils.Foldable           (wrapMaybe)
import Utils.String             (Wrapper (..), joinMaybe, joinWords, str,
                                 wrapParens, wrapParensCsv, wrapSpaces,
                                 wrapSquareCsv, (+++))


newtype TypeParam
  = TypeParam String

newtype TypeVar
  = TypeVar String


data TypeCtor
  = TypeCtor String
  | Arrow
  | TupleType

data Type
  = CtorTypeApply QTypeCtor [Type]
  | ParamTypeApply TypeParam [Type]
  | NestedTypeApply Type [Type]
  | TypeVar' QTypeVar
  | TypeParam' TypeParam
  | ExistentialType
  | TypeScope [TypeParam] Type
  | ClassScope [ClassConstraint] Type


data ClassConstraint
  = ClassConstraint QTypeClass [Type]

newtype ArgList
  = ArgList [ArgField]

newtype UsingArgList
  = UsingArgList [UsingArgField]


data ArgField
  = ArgField
      { modifiers :: [Modifier]
      , name      :: Var
      , type'     :: Type
      }

data UsingArgField
  = UsingArgField
      { modifiers :: [Modifier]
      , name      :: Maybe Var
      , type'     :: ClassConstraint
      }

data QTypeVar
  = QTypeVar (Maybe Package) TypeVar

data QTypeCtor
  = QTypeCtor (Maybe Package) TypeCtor



instance Show TypeParam where
  show (TypeParam x) = x

instance Show TypeVar where
  show (TypeVar x) = x

instance Show TypeCtor where
  show (TypeCtor x) = x
  show Arrow        = "->"
  show TupleType    = "()"

instance Show Type where
  show (CtorTypeApply (QTypeCtor _ Arrow) x) = intercalate (wrapSpaces "=>") $ showTypeNested <$> x
  show (CtorTypeApply x@(QTypeCtor _ (TypeCtor _)) z) = show x ++ wrapSquareCsv z
  show (CtorTypeApply (QTypeCtor _ TupleType) x)    = wrapParensCsv x
  show (ParamTypeApply x y) = show x ++ wrapSquareCsv y
  show (NestedTypeApply x y) = showTypeNested x ++ wrapSquareCsv y
  show (TypeVar' x) = show x
  show (TypeParam' x) = show x
  show ExistentialType = "?"
  show (TypeScope x y) = wrapSquareCsv x +++ "=>" +++ showTypeScopeNested y
  show (ClassScope x y) = wrapParensCsv x +++ "?=>" +++ showClassScopeNested y


instance Show ArgList where
  show (ArgList x) = wrapParensCsv x

instance Show UsingArgList where
  show (UsingArgList x) = wrapParens $
    "using" `joinMaybe` (Wrapper <$> wrapMaybe (str ", " x))

instance Show ArgField where
  show (ArgField x y z) =
    joinWords [str " " x,
               show y ++ ":",
               show z]

instance Show UsingArgField where
  show (UsingArgField x y z) =
    joinWords [str " " x,
               ":" `joinMaybe` y,
               show z]

instance Show ClassConstraint where
  show (ClassConstraint x y) = show x ++ wrapSquareCsv y


instance Show QTypeVar where
  show (QTypeVar x y) = showQualified x y

instance Show QTypeCtor where
  show (QTypeCtor x y) = showQualified x y


showTypeNested :: Type -> String
showTypeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      (CtorTypeApply (QTypeCtor _ Arrow) _) -> True
      (TypeScope _ _)                       -> True
      (ClassScope _ _)                      -> True
      _                                     -> False

showTypeScopeNested :: Type -> String
showTypeScopeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      (TypeScope _ _) -> True
      _               -> False

showClassScopeNested :: Type -> String
showClassScopeNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      (TypeScope _ _)  -> True
      (ClassScope _ _) -> True
      _                -> False
