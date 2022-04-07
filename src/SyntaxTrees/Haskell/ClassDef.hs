module SyntaxTrees.Haskell.ClassDef  where

import SyntaxTrees.Haskell.Common ( Class )
import SyntaxTrees.Haskell.Type ( ClassConstraint, AnyKindedType, TypeParam )
import SyntaxTrees.Haskell.FnDef ( FnDefOrSig )


data ClassDef = ClassDef {
    constraints :: [ClassConstraint]
  , name        :: Class
  , typeParams  :: [TypeParam]
  , defs        :: [FnDefOrSig]
}

data InstanceDef = InstanceDef {
    constraints :: [ClassConstraint]
  , class'      :: Class
  , types       :: [AnyKindedType]
  , defs        :: [FnDefOrSig]
}
