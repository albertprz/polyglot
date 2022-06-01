module SyntaxTrees.Haskell.ModuleDef where

import           SyntaxTrees.Haskell.ClassDef (ClassDef, InstanceDef)
import           SyntaxTrees.Haskell.Common   (Module, Var)
import           SyntaxTrees.Haskell.DataDef  (DataDef, NewTypeDef, TypeDef)
import           SyntaxTrees.Haskell.FnDef    (FnDef, FnSig)
import           SyntaxTrees.Haskell.Type     (Type, TypeVar)


data ModuleDef = ModuleDef
  { name    :: Module,
    export :: Maybe ModuleExport,
    imports :: [ModuleImport],
    defs    :: [InternalDef]
  }

newtype ModuleExport
  = ModuleExport [ModuleExportDef]

data ModuleExportDef
  = FnExport Var
  | DataExport TypeVar
  | FullDataExport TypeVar
  | FilteredDataExport TypeVar [Var]

data ModuleImport
  = ModuleImport Module [ModuleImportDef]

data ModuleImportDef
  = FnImport Var
  | DataImport TypeVar
  | FullDataImport TypeVar
  | FilteredDataImport TypeVar [Var]

data InternalDef
  = TypeDef' TypeDef
  | NewTypeDef' NewTypeDef
  | DataDef' DataDef
  | FnDef' FnDef
  | FnSig' FnSig
  | ClassDef' ClassDef
  | InstanceDef' InstanceDef
