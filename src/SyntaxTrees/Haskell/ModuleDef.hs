module SyntaxTrees.Haskell.ModuleDef  where

import SyntaxTrees.Haskell.Term ( TermName )
import SyntaxTrees.Haskell.DataDef ( TypeDef,  NewTypeDef, DataDef )
import SyntaxTrees.Haskell.FnDef ( FnDef )
import SyntaxTrees.Haskell.ClassDef ( ClassDef, InstanceDef )


data ModuleDef = ModuleDef  {
    moduleName :: TermName
  , exports    :: ModuleExport
  , imports    :: [ModuleImport]
  , defs       :: [InternalDef]
}

newtype ModuleExport = ModuleExport [ModuleDefExport]
data ModuleDefExport = SingleExport TermName |
                       FullDataExport TermName |
                       FilteredDataExport TermName [TermName]

data ModuleImport    = ModuleImport TermName [ModuleDefImport]
data ModuleDefImport = SingleImport TermName |
                       FullDataImport TermName |
                       FilteredDataImport TermName [TermName]

data InternalDef = Type TypeDef |
                   NewType NewTypeDef |
                   Data DataDef |
                   Fn FnDef |
                   Class ClassDef |
                   Instance InstanceDef
