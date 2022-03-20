module SyntaxTrees.Haskell.ModuleDef  where

import SyntaxTrees.Haskell.Common ( Module, Term, Type )
import SyntaxTrees.Haskell.DataDef ( TypeDef,  NewTypeDef, DataDef )
import SyntaxTrees.Haskell.FnDef ( FnDef, ClassDef, InstanceDef )

data ModuleDef = ModuleDef  {
    name    :: Module
  , exports :: ModuleExport
  , imports :: [ModuleImport]
  , defs    :: [InternalDef]
}

newtype ModuleExport = ModuleExport [ModuleDefExport]
data ModuleDefExport = SingleExport Term |
                       FullDataExport Type |
                       FilteredDataExport Type [Term]

data ModuleImport    = ModuleImport Term [ModuleDefImport]
data ModuleDefImport = SingleImport Term |
                       FullDataImport Type |
                       FilteredDataImport Type [Term]

data InternalDef = TypeDef' TypeDef |
                   NewTypeDef' NewTypeDef |
                   DataDef' DataDef |
                   FnDef' FnDef |
                   ClassDef' ClassDef |
                   InstanceDef' InstanceDef
