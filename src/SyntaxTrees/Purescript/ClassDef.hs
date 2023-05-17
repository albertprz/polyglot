module SyntaxTrees.Purescript.ClassDef where

import SyntaxTrees.Purescript.Common (Class, Var)
import SyntaxTrees.Purescript.FnDef  (FnDefOrSig)
import SyntaxTrees.Purescript.Type   (AnyKindedType, ClassConstraint, TypeParam)


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
      , name        :: Maybe Var
      }
  deriving (Show)

data DerivingDef
  = DerivingDef
      { constraints :: [ClassConstraint]
      , class'      :: Class
      , types       :: [AnyKindedType]
      , derivingVia :: Maybe Class
      , name        :: Maybe Var
      }
  deriving (Show)
