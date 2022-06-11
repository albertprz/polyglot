module SyntaxTrees.Scala.PackageDef where

import SyntaxTrees.Scala.Common  (Package, Var)
import SyntaxTrees.Scala.DataDef (InternalDef)
import SyntaxTrees.Scala.Type    (TypeVar)
import Utils.String


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


instance Show PackageDef where
  show (ModuleDef x y z) = joinLines ["package" +++ show x,
                                      unlines (show <$> y),
                                      joinLines (show <$> z)]

instance Show PackageImport where
  show (PackageImport x y) = "import" +++ show x ++ show y

instance Show PackageImportDef where
  show (MembersImport x) = wrapCurlyCsv x
  show FullImport        = "_"

instance Show PackageMember where
  show (VarMember x)  = show x
  show (DataMember x) = show x
