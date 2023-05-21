module SyntaxTrees.Haskell.ModuleDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef, DerivingDef, InstanceDef)
import SyntaxTrees.Haskell.Common   (Module, Var)
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
  = ModuleImport Bool Module (Maybe Module) Bool [ModuleImportDef]
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
  | InfixFnAnnotation' InfixFnAnnotation
  deriving (Show)

data ModuleMember
  = VarMember Var
  | DataMember TypeVar
  deriving (Show)
