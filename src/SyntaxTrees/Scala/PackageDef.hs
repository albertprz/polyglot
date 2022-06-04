module SyntaxTrees.Scala.PackageDef where

import SyntaxTrees.Scala.Common  (Package, Var)
import SyntaxTrees.Scala.DataDef (InternalDef)
import SyntaxTrees.Scala.Type    (TypeVar)


data PackageDef
  = ModuleDef
      { name    :: Package
      , imports :: [PackageImport]
      , defs    :: [InternalDef]
      }

data PackageImport
  = PackageImport Package PackageImportDef

data PackageImportDef
  = MembersImport [PackageMember]
  | FullImport

data PackageMember
  = VarMember Var
  | DataMember TypeVar
