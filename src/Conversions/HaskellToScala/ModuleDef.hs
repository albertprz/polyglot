module Conversions.HaskellToScala.ModuleDef where

import qualified SyntaxTrees.Haskell.ModuleDef as H
import qualified SyntaxTrees.Scala.DataDef     as S
import qualified SyntaxTrees.Scala.FnDef       as S
import qualified SyntaxTrees.Scala.PackageDef  as S

import Conversions.HaskellToScala.ClassDef (classDef, instanceDef)
import Conversions.HaskellToScala.Common   (module', var)
import Conversions.HaskellToScala.DataDef  (dataDef, newtypeDef, typeDef)
import Conversions.HaskellToScala.FnDef    (fnDefOrSigs, fnDefs)
import Conversions.HaskellToScala.Type     (typeVar)

import Data.Maybe     (mapMaybe, maybeToList)
import Utils.Foldable (wrapMaybe)
import Utils.Maybe    (cond)



moduleDef :: H.ModuleDef -> S.PackageDef
moduleDef (H.ModuleDef x _ z t) =
  S.PackageDef (module' x) (moduleImport =<< z) (internalDefs t)


moduleImport :: H.ModuleImport -> [S.PackageImport]
moduleImport (H.ModuleImport x y z []) =
  [S.PackageImport (module' y) (module' <$> z)
   (cond (not x) S.FullImport)]
moduleImport (H.ModuleImport x y z t) =
  S.PackageImport (module' y) (module' <$> z) . (cond (not x))
  <$> moduleImportDefs t

moduleImportDefs :: [H.ModuleImportDef] -> [S.PackageImportDef]
moduleImportDefs importDefs = maybeToList combined ++ singles
  where
    combined = S.MembersImport <$> wrapMaybe (mapMaybe singleImportDef importDefs)
    singles = mapMaybe complexImportDef importDefs


complexImportDef :: H.ModuleImportDef -> Maybe S.PackageImportDef
complexImportDef (H.FullDataImport x)       =
  Just $ S.FullObjectImport $ typeVar x
complexImportDef (H.FilteredDataImport x y) =
  S.FilteredObjectImport (typeVar x) <$> wrapMaybe (moduleMember <$> y)
complexImportDef _ = Nothing


singleImportDef :: H.ModuleImportDef -> Maybe S.PackageMember
singleImportDef (H.FnImport x)   = Just $ S.VarMember $ var x
singleImportDef (H.DataImport x) = Just $ S.DataMember $ typeVar x
singleImportDef _                = Nothing


internalDefs :: [H.InternalDef] -> [S.InternalDef]
internalDefs defs =
  (mapMaybe internalDef others) ++
  (S.Fn . fnDefs <$> fnDefOrSigs fns)

  where
    fns    = mapMaybe (\case (H.FnDefOrSig' x) -> Just x
                             _                 -> Nothing) defs

    others = mapMaybe (\case (H.FnDefOrSig' _) -> Nothing
                             x                 -> Just x) defs

internalDef :: H.InternalDef -> Maybe S.InternalDef
internalDef (H.TypeDef' x)           = Just $ S.TypeAlias $ typeDef x
internalDef (H.NewTypeDef' x)        = Just $ S.OpaqueType $ newtypeDef x
internalDef (H.DataDef' x)           = Just $ S.Enum $ dataDef x
internalDef (H.ClassDef' x)          = Just $ S.Trait $ classDef x
internalDef (H.InstanceDef' x)       = Just $ S.Fn $ S.FnGiven $ instanceDef x
internalDef (H.FnDefOrSig' _)        = Nothing
internalDef (H.InfixFnAnnotation' _) = Nothing

moduleMember :: H.ModuleMember -> S.PackageMember
moduleMember (H.VarMember x)  = S.VarMember $ var x
moduleMember (H.DataMember x) = S.DataMember $ typeVar x
