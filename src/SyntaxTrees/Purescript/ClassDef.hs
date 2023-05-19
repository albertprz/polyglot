module SyntaxTrees.Purescript.ClassDef where

import Data.List                     (intercalate)
import SyntaxTrees.Purescript.Common (Class, Var)
import SyntaxTrees.Purescript.FnDef  (FnDefOrSig)
import SyntaxTrees.Purescript.Type   (AnyKindedType, ClassConstraint, TypeParam,
                                      showAnyKindedTypeNested)
import Utils.Foldable                (wrapMaybe)
import Utils.String                  (Wrapper (Wrapper), joinMaybe,
                                      joinMaybePost, joinWords, str, wrapBlock,
                                      wrapParensCsv)


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
      { constraints :: [ClassConstraint]
      , name        :: Maybe Var
      , class'      :: Class
      , types       :: [AnyKindedType]
      , derivingVia :: Maybe Class
      }


instance Show ClassDef where
  show (ClassDef x y z t) =
    joinWords ["class",
               (Wrapper <$> wrapMaybe (wrapParensCsv x))
                `joinMaybePost` "<=",
               show y,
               str " " z,
               wrapBlock t]

instance Show InstanceDef where
  show (InstanceDef x y z t u) =
    joinWords ["instance",
               (Wrapper <$> wrapMaybe (wrapParensCsv x))
                `joinMaybePost` "<=",
               y `joinMaybePost` "::",
               show z,
               intercalate " " $ showAnyKindedTypeNested <$> t,
               wrapBlock u]

instance Show DerivingDef where
  show (DerivingDef x y z t u) =
    joinWords ["deriving",
               (Wrapper <$> wrapMaybe (wrapParensCsv x)) `joinMaybePost` "<=",
               y `joinMaybePost` "::",
               show z,
               intercalate " " $ showAnyKindedTypeNested <$> t,
               "via" `joinMaybe` u]
