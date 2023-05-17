module SyntaxTrees.Purescript.ModuleDef where

import SyntaxTrees.Purescript.ClassDef (ClassDef, InstanceDef, DerivingDef)
import SyntaxTrees.Purescript.Common   (Module, Var)
import SyntaxTrees.Purescript.DataDef  (DataDef, NewTypeDef, TypeDef)
import SyntaxTrees.Purescript.FnDef    (FnDefOrSig, InfixFnDef)
import SyntaxTrees.Purescript.Type     (TypeVar)


data ModuleDef
  = ModuleDef
      { name    :: Module
      , export  :: Maybe ModuleExport
      , imports :: [ModuleImport]
      , defs    :: [InternalDef]
      }
  deriving (Show)

newtype ModuleExport
  = ModuleExport [ModuleExportDef]
  deriving (Show)

data ModuleExportDef
  = FnExport Var
  | DataExport TypeVar
  | FullDataExport TypeVar
  | FilteredDataExport TypeVar [ModuleMember]
  deriving (Show)

data ModuleImport
  = ModuleImport Bool Module (Maybe Module) [ModuleImportDef]
  deriving (Show)

data ModuleImportDef
  = FnImport Var
  | DataImport TypeVar
  | FullDataImport TypeVar
  | FilteredDataImport TypeVar [ModuleMember]
  deriving (Show)

data InternalDef
  = TypeDef' TypeDef
  | NewTypeDef' NewTypeDef
  | DataDef' DataDef
  | FnDefOrSig' FnDefOrSig
  | ClassDef' ClassDef
  | InstanceDef' InstanceDef
  | DerivingDef' DerivingDef
  | InfixFnDef' InfixFnDef
  deriving (Show)

data ModuleMember
  = VarMember Var
  | DataMember TypeVar
  deriving (Show)
