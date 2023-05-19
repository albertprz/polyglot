module SyntaxTrees.Scala.DataDef where

import Data.Monoid.HT           (when)
import SyntaxTrees.Scala.Common (Ctor, Modifier, TypeClass)
import SyntaxTrees.Scala.FnDef  (InternalFnDef, MethodDef)
import SyntaxTrees.Scala.Type   (ArgList (..), Type, TypeCtor, TypeParam,
                                 UsingArgList)
import Utils.Foldable           (hasSome)
import Utils.String             (Empty (Empty), joinList, joinWords, str,
                                 wrapSpacedBlock, wrapSquareCsv)


data TypeDef
  = TypeDef
      { alias      :: TypeCtor
      , typeParams :: [TypeParam]
      , type'      :: Type
      }

data OpaqueTypeDef
  = OpaqueTypeDef
      { alias      :: TypeCtor
      , typeParams :: [TypeParam]
      , type'      :: Type
      , derives    :: [TypeClass]
      }

data TraitDef
  = TraitDef
      { modifiers     :: [Modifier]
      , name          :: TypeCtor
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , internalDefs  :: [InternalDef]
      }

data ClassDef
  = ClassDef
      { modifiers     :: [Modifier]
      , name          :: TypeCtor
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , internalDefs  :: [InternalDef]
      }

data ObjectDef
  = ObjectDef
      { modifiers    :: [Modifier]
      , name         :: TypeCtor
      , extends      :: [Type]
      , internalDefs :: [InternalDef]
      }


data EnumDef
  = EnumDef
      { modifiers     :: [Modifier]
      , name          :: TypeCtor
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , derives       :: [TypeClass]
      , cases         :: [EnumCaseDef]
      }

data EnumCaseDef
  = EnumCaseDef
      { name          :: Ctor
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , internalDefs  :: [InternalFnDef]
      }


data CaseClassDef
  = CaseClassDef
      { modifiers     :: [Modifier]
      , name          :: TypeCtor
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , derives       :: [TypeClass]
      , extends       :: [Type]
      , internalDefs  :: [InternalFnDef]
      }

data CaseObjectDef
  = CaseObjectDef
      { modifiers    :: [Modifier]
      , name         :: TypeCtor
      , derives      :: [TypeClass]
      , extends      :: [Type]
      , internalDefs :: [InternalFnDef]
      }

data ExtensionDef
  = ExtensionDef
      { typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , methodDefs    :: [MethodDef]
      }


data InternalDef
  = Fn InternalFnDef
  | TypeAlias TypeDef
  | OpaqueType OpaqueTypeDef
  | Trait TraitDef
  | Class ClassDef
  | Object ObjectDef
  | Enum EnumDef
  | EnumCase EnumCaseDef
  | CaseClass CaseClassDef
  | CaseObject CaseObjectDef
  | Extension ExtensionDef


instance Show TypeDef where
  show (TypeDef x y z) =
    joinWords ["type",
               show x,
               wrapSquareCsv y,
               "=",
               show z]

instance Show OpaqueTypeDef where
  show (OpaqueTypeDef x y z t) =
    joinWords ["opaque type",
               show x,
               wrapSquareCsv y,
               "=",
               show z,
               joinList "derives" "," t]

instance Show TraitDef where
  show (TraitDef x y z t u v w) =
    showStructure "trait" x y z t u [] v w

instance Show ClassDef where
  show (ClassDef x y z t u v w) =
    showStructure "class" x y z t u [] v w

instance Show ObjectDef where
  show (ObjectDef x y z t) =
    showStructure "object" x y [] [] [] [] z t

instance Show EnumDef where
  show (EnumDef x y z t u v w) =
    showStructure "enum" x y z t u v [] w

instance Show EnumCaseDef where
  show (EnumCaseDef x y z t u) =
    showStructure "case" [] x [] y z [] t u

instance Show CaseClassDef where
  show (CaseClassDef x y z t u v w r) =
    showStructure "case class" x y z t u v w r

instance Show CaseObjectDef where
  show (CaseObjectDef x y z t u) =
    showStructure "case object" x y [] [] [] z t u

instance Show ExtensionDef where
  show (ExtensionDef x y z t) =
    showStructure "extension" [] Empty x y z [] [] t



showStructure :: (Show a, Show b) => String -> [Modifier] -> a -> [TypeParam]
                 -> [ArgList] -> [UsingArgList] -> [TypeClass] -> [Type] -> [b]
                 -> String
showStructure  x y z t u v w r s =
  joinWords [x,
             str " " y,
             show z,
             wrapSquareCsv t,
             str " " u,
             str " " v,
             joinList "derives" ", " w,
             joinList "extends" ", " r,
             sep ++ wrapSpacedBlock s]
  where
    sep = when (x /= "extension" && hasSome s) ": "

instance Show InternalDef where
  show (Fn x)         = show x
  show (TypeAlias x)  = show x
  show (OpaqueType x) = show x
  show (Trait x)      = show x
  show (Class x)      = show x
  show (Object x)     = show x
  show (Enum x)       = show x
  show (EnumCase x)   = show x
  show (CaseClass x)  = show x
  show (CaseObject x) = show x
  show (Extension x)  = show x
