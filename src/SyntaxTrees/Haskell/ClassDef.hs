module SyntaxTrees.Haskell.ClassDef where

import SyntaxTrees.Haskell.Common  (Class)
import SyntaxTrees.Haskell.DataDef (DerivingStrategy)
import SyntaxTrees.Haskell.FnDef   (FnDefOrSig)
import SyntaxTrees.Haskell.Type    (AnyKindedType, ClassConstraint, TypeParam)


data ClassDef
  = ClassDef
      { constraints :: [ClassConstraint]
      , name        :: Class
      , typeParams  :: [TypeParam]
      , defs        :: [FnDefOrSig]
      }
  deriving (Show)

data InstanceDef
  = InstanceDef
      { constraints :: [ClassConstraint]
      , class'      :: Class
      , types       :: [AnyKindedType]
      , defs        :: [FnDefOrSig]
      }
  deriving (Show)

data DerivingDef
  = DerivingDef
      { strategy    :: DerivingStrategy
      , constraints :: [ClassConstraint]
      , class'      :: Class
      , types       :: [AnyKindedType]
      , derivingVia :: Maybe Class
      }
  deriving (Show)
