module SyntaxTrees.Scala.DataDef where

import SyntaxTrees.Scala.Common (Modifier)
import SyntaxTrees.Scala.FnDef  (InternalFnDef, MethodDef, ValDef, VarDef)
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
      , derives      :: [Type]
      , internalDefs :: [InternalFnDef]
      }
  deriving (Show)


data TraitDef
  = TraitDef
      { modifiers    :: [Modifier]
      , name         :: Type
      , typeParams   :: [TypeParam]
      , argLists     :: [ArgList]
      , usingArgs    :: UsingArgList
      , extends      :: [Type]
      , internalDefs :: [InternalDef]
      }
  deriving (Show)

data ClassDef
  = ClassDef
      { modifiers    :: [Modifier]
      , name         :: Type
      , typeParams   :: [TypeParam]
      , argLists     :: [ArgList]
      , usingArgs    :: UsingArgList
      , extends      :: [Type]
      , internalDefs :: [InternalDef]
      }
  deriving (Show)

data ObjectDef
  = ObjectDef
      { modifiers    :: [Modifier]
      , name         :: Type
      , extends      :: [Type]
      , internalDefs :: [InternalDef]
      }
  deriving (Show)


data EnumDef
  = EnumDef
      { modifiers  :: [Modifier]
      , name       :: Type
      , typeParams :: [TypeParam]
      , argLists   :: [ArgList]
      , usingArgs  :: UsingArgList
      , extends    :: [Type]
      , derives    :: [Type]
      , cases      :: [EnumCaseDef]
      }
  deriving (Show)

data EnumCaseDef
  = EnumCaseDef
      { name         :: Type
      , argLists     :: [ArgList]
      , usingArgs    :: UsingArgList
      , extends      :: [Type]
      , internalDefs :: [InternalFnDef]
      }
  deriving (Show)


data CaseClassDef
  = CaseClassDef
      { modifiers    :: [Modifier]
      , name         :: Type
      , typeParams   :: [TypeParam]
      , argLists     :: [ArgList]
      , usingArgs    :: UsingArgList
      , extends      :: [Type]
      , derives      :: [Type]
      , internalDefs :: [InternalFnDef]
      }
  deriving (Show)

data CaseObjectDef
  = CaseObjectDef
      { modifiers    :: [Modifier]
      , name         :: Type
      , extends      :: [Type]
      , derives      :: [Type]
      , internalDefs :: [InternalFnDef]
      }
  deriving (Show)

data ExtensionDef
  = ExtensionDef
      { typeParams :: [TypeParam]
      , argLists   :: [ArgList]
      , usingArgs  :: UsingArgList
      , methodDefs :: [MethodDef]
      }
  deriving (Show)


data InternalDef
  = Var VarDef
  | Val ValDef
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
