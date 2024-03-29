module SyntaxTrees.Purescript.ClassDef where

import ClassyPrelude

import Data.Monoid.Extra             (mwhen)
import SyntaxTrees.Purescript.Common (Class, Var)
import SyntaxTrees.Purescript.FnDef  (FnDefOrSig)
import SyntaxTrees.Purescript.Type   (AnyKindedType, ClassConstraint, TypeParam,
                                      showAnyKindedTypeNested)
import Utils.Foldable                (wrapMaybe)
import Utils.String                  (Wrapper (Wrapper), joinMaybePost,
                                      joinWords, str, wrapBlock, wrapParensCsv)


data ClassDef
  = ClassDef
      { constraints :: [ClassConstraint]
      , name        :: Class
      , typeParams  :: [TypeParam]
      , defs        :: [FnDefOrSig]
      }

data InstanceDef
  = InstanceDef
      { constraints :: [ClassConstraint]
      , name        :: Maybe Var
      , class'      :: Class
      , types       :: [AnyKindedType]
      , defs        :: [FnDefOrSig]
      }

data DerivingDef
  = DerivingDef
      { strategy    :: DerivingStrategy
      , constraints :: [ClassConstraint]
      , name        :: Maybe Var
      , class'      :: Class
      , types       :: [AnyKindedType]
      }

data DerivingStrategy
  = StandardDeriving
  | NewTypeDeriving
  deriving (Eq)


instance Show ClassDef where
  show (ClassDef x y z t) =
    joinWords ["class",
               (Wrapper <$> wrapMaybe (wrapParensCsv x))
                `joinMaybePost` "<=",
               show y,
               str " " z,
               "where",
               wrapBlock t]

instance Show InstanceDef where
  show (InstanceDef x y z t u) =
    joinWords ["instance",
               (Wrapper <$> wrapMaybe (wrapParensCsv x))
                `joinMaybePost` "=>",
               y `joinMaybePost` "::",
               show z,
               unwords $ showAnyKindedTypeNested <$> t,
               "where",
               wrapBlock u]

instance Show DerivingDef where
  show (DerivingDef x y z t u) =
    joinWords ["derive",
               mwhen (x == NewTypeDeriving) "newtype",
               "instance",
               (Wrapper <$> wrapMaybe (wrapParensCsv y)) `joinMaybePost` "=>",
               z `joinMaybePost` "::",
               show t,
               unwords $ showAnyKindedTypeNested <$> u]
