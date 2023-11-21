module SyntaxTrees.Purescript.DataDef where

import ClassyPrelude

import SyntaxTrees.Purescript.Common (Ctor, Var)
import SyntaxTrees.Purescript.Type   (AnyKindedType, Type, TypeCtor, TypeParam,
                                      showTypeNested)
import Utils.String                  (joinWords, str, wrapCurlyCsv, wrapSpaces)


data TypeDef
  = TypeDef
      { alias      :: TypeCtor
      , typeParams :: [TypeParam]
      , type'      :: AnyKindedType
      }

data NewTypeDef
  = NewTypeDef
      { type'      :: TypeCtor
      , typeParams :: [TypeParam]
      , ctor       :: Ctor
      , field      :: FieldDef
      }

data DataDef
  = DataDef
      { type'      :: TypeCtor
      , typeParams :: [TypeParam]
      , ctorDefs   :: [DataCtorDef]
      }

data DataCtorDef
  = UnNamedFieldsCtor
      { ctor          :: Ctor
      , unnamedFields :: [UnNamedFieldDef]
      }
  | NamedFieldsCtor
      { ctor        :: Ctor
      , namedFields :: [NamedFieldDef]
      }

data FieldDef
  = UnNamedField UnNamedFieldDef
  | NamedField NamedFieldDef

data UnNamedFieldDef
  = UnNamedFieldDef
      { type' :: Type
      }

data NamedFieldDef
  = NamedFieldDef
      { name  :: Var
      , type' :: Type
      }


instance Show TypeDef where
  show (TypeDef x y z) =
    joinWords ["type",
               show x,
               str " " y,
               "=",
               show z]

instance Show NewTypeDef where
  show (NewTypeDef x y z t) =
    joinWords ["newtype",
               show x,
               str " " y,
               "=",
               show z,
               show t]

instance Show DataDef where
  show (DataDef x y z) =
    joinWords ["data",
               show x,
               str " " y,
               "=",
               str (wrapSpaces "|") z]

instance Show DataCtorDef where
  show (UnNamedFieldsCtor x y) =
    joinWords [show x,
               str " " y]

  show (NamedFieldsCtor x y) =
    joinWords [show x,
               wrapCurlyCsv y]

instance Show FieldDef where
  show (UnNamedField x) = show x
  show (NamedField x)   = show x

instance Show UnNamedFieldDef where
  show (UnNamedFieldDef x) = showTypeNested x

instance Show NamedFieldDef where
  show (NamedFieldDef x y) =
    joinWords [show x,
               "::",
               show y]
