module SyntaxTrees.Haskell.ClassDef  where

import SyntaxTrees.Haskell.Common ( AnyKindedType, TypeParam,
                                    Class, ClassConstraint )
import SyntaxTrees.Haskell.FnDef ( FnDef, FnSig )


data ClassDef = ClassDef {
    name        :: Class
  , constraints :: [ClassConstraint]
  , typeParam   :: [TypeParam]
  , sigs        :: [FnSig]
}

data InstanceDef = InstanceDef {
    class' :: Class
  , type'  :: AnyKindedType
  , defs   :: FnDef
}
