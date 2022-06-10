module SyntaxTrees.Scala.DataDef where

import SyntaxTrees.Scala.Common (Ctor, Modifier, TypeClass)
import SyntaxTrees.Scala.FnDef  (InternalFnDef, MethodDef, ValDef)
import SyntaxTrees.Scala.Type   (AnyKindedType, ArgField, ArgList, Type,
                                 TypeParam, TypeVar, UsingArgList)


data TypeDef
  = TypeDef
      { alias      :: TypeVar
      , typeParams :: [TypeParam]
      , type'      :: AnyKindedType
      }
  deriving (Show)

data OpaqueTypeDef
  = OpaqueTypeDef
      { type'        :: TypeVar
      , typeParams   :: [TypeParam]
      , field        :: ArgField
      , derives      :: [TypeClass]
      , internalDefs :: [InternalFnDef]
      }
  deriving (Show)


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
  deriving (Show)

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
  deriving (Show)

data ObjectDef
  = ObjectDef
      { modifiers    :: [Modifier]
      , name         :: TypeVar
      , extends      :: [Type]
      , internalDefs :: [InternalDef]
      }
  deriving (Show)


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
  deriving (Show)

data EnumCaseDef
  = EnumCaseDef
      { name          :: Ctor
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , internalDefs  :: [InternalFnDef]
      }
  deriving (Show)


data CaseClassDef
  = CaseClassDef
      { modifiers     :: [Modifier]
      , name          :: TypeVar
      , typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , extends       :: [Type]
      , derives       :: [TypeClass]
      , internalDefs  :: [InternalFnDef]
      }
  deriving (Show)

data CaseObjectDef
  = CaseObjectDef
      { modifiers    :: [Modifier]
      , name         :: TypeVar
      , extends      :: [Type]
      , derives      :: [Type]
      , internalDefs :: [InternalFnDef]
      }
  deriving (Show)

data ExtensionDef
  = ExtensionDef
      { typeParams    :: [TypeParam]
      , argLists      :: [ArgList]
      , usingArgLists :: [UsingArgList]
      , methodDefs    :: [MethodDef]
      }
  deriving (Show)


data InternalDef
  = Val ValDef
  | Method MethodDef
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
  deriving (Show)
