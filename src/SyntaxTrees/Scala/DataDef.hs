module SyntaxTrees.Scala.DataDef where

import SyntaxTrees.Scala.Common (Ctor, Modifier, TypeClass)
import SyntaxTrees.Scala.FnDef  (GivenDef, InternalFnDef, MethodDef, ValDef)
import SyntaxTrees.Scala.Type   (ArgField, ArgList (..), Type, TypeParam,
                                 TypeVar, UsingArgList)
import Utils.String


data TypeDef
  = TypeDef
      { alias      :: TypeVar
      , typeParams :: [TypeParam]
      , type'      :: Type
      }

data OpaqueTypeDef
  = OpaqueTypeDef
      { type'        :: TypeVar
      , typeParams   :: [TypeParam]
      , field        :: ArgField
      , derives      :: [TypeClass]
      , internalDefs :: [InternalFnDef]
      }

data TraitDef
  = TraitDef
      { modifiers     :: [Modifier]
      , name          :: TypeVar
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , internalDefs  :: [InternalDef]
      }

data ClassDef
  = ClassDef
      { modifiers     :: [Modifier]
      , name          :: TypeVar
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , internalDefs  :: [InternalDef]
      }

data ObjectDef
  = ObjectDef
      { modifiers    :: [Modifier]
      , name         :: TypeVar
      , extends      :: [Type]
      , internalDefs :: [InternalDef]
      }


data EnumDef
  = EnumDef
      { modifiers     :: [Modifier]
      , name          :: TypeVar
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
      , name          :: TypeVar
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
      , name         :: TypeVar
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
  = Val ValDef
  | Method MethodDef
  | Given GivenDef
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
    joinWords ["type", show x, wrapSquareCsv y, "=", show z]

instance Show OpaqueTypeDef where
  show (OpaqueTypeDef x y z t u) =
    showStructure "opaque type" [] x y [ArgList [z]] [] t [] u

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
showStructure  x y z t u v w r s = joinWords [x,
                                              str " " y,
                                              show z,
                                              wrapSquareCsv t,
                                              str " " u,
                                              str " " v,
                                              joinList "derives" ", " w,
                                              joinList "extends" ", " r ++ ":",
                                              wrapSpacedBlock s]

instance Show InternalDef where
  show (Val x)        = show x
  show (Method x)     = show x
  show (Given x)      = show x
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


data Empty
  = Empty

instance Show Empty where
  show Empty = ""
