module SyntaxTrees.Scala.PackageDef where

import Data.List                 (intercalate)
import SyntaxTrees.Scala.Common  (Package, Var)
import SyntaxTrees.Scala.DataDef (InternalDef)
import SyntaxTrees.Scala.Type    (TypeVar)
import Utils.String


data PackageDef
  = PackageDef
      { name    :: Package
      , imports :: [PackageImport]
      , defs    :: [InternalDef]
      }

data PackageImport
  = PackageImport Package PackageImportDef

data PackageImportDef
  = FullImport
  | MembersImport [PackageMember]
  | FullObjectImport TypeVar
  | FilteredObjectImport TypeVar [PackageMember]

data PackageMember
  = VarMember Var
  | DataMember TypeVar


instance Show PackageDef where
  show (PackageDef x y z) = joinLines ["package" +++ show x,
                                      intercalate "\n" (show <$> y),
                                      joinLines (show <$> z)]

instance Show PackageImport where
  show (PackageImport x y) = "import" +++ show x ++ "." ++ show y

instance Show PackageImportDef where
  show FullImport                   = "_"
  show (MembersImport [x])          = show x
  show (MembersImport x)            = wrapCurlyCsv x
  show (FullObjectImport x)         = show x ++ "." ++ "_"
  show (FilteredObjectImport x [y]) = show x ++ "." ++ show y
  show (FilteredObjectImport x y)   = show x ++ "." ++ wrapCurlyCsv y

instance Show PackageMember where
  show (VarMember x)  = show x
  show (DataMember x) = show x
