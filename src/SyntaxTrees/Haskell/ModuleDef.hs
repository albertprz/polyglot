module SyntaxTrees.Haskell.ModuleDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef, InstanceDef)
import SyntaxTrees.Haskell.Common (Module, Var)
import SyntaxTrees.Haskell.DataDef (DataDef, NewTypeDef, TypeDef)
import SyntaxTrees.Haskell.FnDef (FnDef, FnSig)
import SyntaxTrees.Haskell.Type (Type)

data ModuleDef = ModuleDef
  { name :: Module,
    exports :: ModuleExport,
    imports :: [ModuleImport],
    defs :: [InternalDef]
  }

newtype ModuleExport = ModuleExport [ModuleDefExport]

data ModuleDefExport
  = SingleExport Var
  | FullDataExport Type
  | FilteredDataExport Type [Var]

data ModuleImport = ModuleImport Var [ModuleDefImport]

data ModuleDefImport
  = SingleImport Var
  | FullDataImport Type
  | FilteredDataImport Type [Var]

data InternalDef
  = TypeDef' TypeDef
  | NewTypeDef' NewTypeDef
  | DataDef' DataDef
  | FnDef' FnDef
  | FnSig' FnSig
  | ClassDef' ClassDef
  | InstanceDef' InstanceDef
