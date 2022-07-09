module SyntaxTrees.Scala.Type where

import SyntaxTrees.Scala.Common (Modifier, Package, QTypeClass, Var,
                                 Wrapper (..), showQualified)
import Utils.Foldable           (wrapMaybe)
import Utils.String             (joinMaybe, joinWords, str, wrapParens,
                                 wrapParensCsv, wrapSpaces, wrapSquareCsv,
                                 (+++))


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
  show (CtorTypeApply x@(QTypeCtor _ (TypeCtor _)) z) = show x ++ wrapSquareCsv z
  show (CtorTypeApply (QTypeCtor _ Arrow) x)        = str (wrapSpaces "=>") x
  show (CtorTypeApply (QTypeCtor _ TupleType) x)    = wrapParensCsv x
  show (ParamTypeApply x y) = show x ++ wrapSquareCsv y
  show (NestedTypeApply x y) = show x ++ wrapSquareCsv y
  show (TypeVar' x) = show x
  show (TypeParam' x) = show x
  show ExistentialType = "?"
  show (TypeScope x y) = wrapParens (wrapSquareCsv x +++ "=>" +++ show y)
  show (ClassScope x y) = wrapParens (wrapParensCsv x +++ "?=>" +++ show y)


instance Show ArgList where
  show (ArgList x) = wrapParensCsv x

instance Show UsingArgList where
  show (UsingArgList x) = wrapParens $ "using" `joinMaybe` (Wrapper <$>
                                                            wrapMaybe ( str ", " x))

instance Show ArgField where
  show (ArgField x y z) = joinWords [str " " x, show y ++ ":", show z]

instance Show UsingArgField where
  show (UsingArgField x y z) = joinWords [str " " x, ":" `joinMaybe` y, show z]

instance Show ClassConstraint where
  show (ClassConstraint x y) = show x ++ wrapSquareCsv y


instance Show QTypeVar where
  show (QTypeVar x y) = showQualified x y

instance Show QTypeCtor where
  show (QTypeCtor x y) = showQualified x y
