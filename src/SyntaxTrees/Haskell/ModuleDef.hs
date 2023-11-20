module SyntaxTrees.Haskell.ModuleDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef, DerivingDef, InstanceDef)
import SyntaxTrees.Haskell.Common   (Module, Var, VarOp)
import SyntaxTrees.Haskell.DataDef  (DataDef, NewTypeDef, TypeDef)
import SyntaxTrees.Haskell.FnDef    (FnDefOrSig, InfixFnAnnotation)
import SyntaxTrees.Haskell.Type     (TypeVar)


data ModuleDef
  = ModuleDef
      { name    :: Module
      , export  :: Maybe ModuleExport
      , imports :: [ModuleImport]
      , defs    :: [InternalDef]
      }

newtype ModuleExport
  = ModuleExport [ModuleExportDef]

data ModuleExportDef
  = ModuleExportDef ImportExportDef
  | FullModuleExport Module

data ModuleImport
  = ModuleImport
      { qualified  :: Bool
      , module'    :: Module
      , alias      :: Maybe Module
      , hiding     :: Bool
      , importDefs :: [ModuleImportDef]
      }

newtype ModuleImportDef
  = ModuleImportDef ImportExportDef

data ImportExportDef
  = Member ModuleMember
  | FullData TypeVar
  | FilteredData TypeVar [ModuleMember]

data ModuleMember
  = VarMember Var
  | VarOpMember VarOp
  | DataMember TypeVar

data InternalDef
  = TypeDef' TypeDef
  | NewTypeDef' NewTypeDef
  | DataDef' DataDef
  | FnDefOrSig' FnDefOrSig
  | ClassDef' ClassDef
  | InstanceDef' InstanceDef
  | DerivingDef' DerivingDef
  | InfixFnAnnotation' InfixFnAnnotation
