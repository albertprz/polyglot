module Conversions.ToPurescript.ModuleDef where

import qualified SyntaxTrees.Haskell.ModuleDef    as H
import qualified SyntaxTrees.Purescript.ModuleDef as P

import Conversions.ToPurescript.ClassDef (classDef, derivingDef, instanceDef)
import Conversions.ToPurescript.Common   (module', var)
import Conversions.ToPurescript.DataDef  (dataDef, newtypeDef, typeDef)
import Conversions.ToPurescript.FnDef    (fnDefOrSig, infixFnAnnotation)
import Conversions.ToPurescript.Type     (typeVar)



moduleDef :: H.ModuleDef -> P.ModuleDef
moduleDef (H.ModuleDef x y z t) =
  P.ModuleDef (module' x) (moduleExport <$> y)
              (moduleImport <$> z) (foldMap internalDef t)

moduleExport :: H.ModuleExport -> P.ModuleExport
moduleExport (H.ModuleExport x) =
  P.ModuleExport $ moduleExportDef <$> x

moduleExportDef :: H.ModuleExportDef -> P.ModuleExportDef
moduleExportDef (H.FnExport x) = P.FnExport $ var x
moduleExportDef (H.DataExport x) = P.DataExport $ typeVar x
moduleExportDef (H.FullDataExport x) = P.FullDataExport $ typeVar x
moduleExportDef (H.FilteredDataExport x y) =
  P.FilteredDataExport (typeVar x) (moduleMember <$> y)

moduleImport :: H.ModuleImport -> P.ModuleImport
moduleImport (H.ModuleImport _ x y z t) =
  P.ModuleImport (module' x) (module' <$> y) z (moduleImportDef <$> t)


moduleImportDef :: H.ModuleImportDef -> P.ModuleImportDef
moduleImportDef (H.FnImport x) = P.FnImport $ var x
moduleImportDef (H.DataImport x) = P.DataImport $ typeVar x
moduleImportDef (H.FullDataImport x) = P.FullDataImport $ typeVar x
moduleImportDef (H.FilteredDataImport x y) =
  P.FilteredDataImport (typeVar x) (moduleMember <$> y)


internalDef :: H.InternalDef -> [P.InternalDef]
internalDef (H.TypeDef' x)           = [P.TypeDef' $ typeDef x]
internalDef (H.NewTypeDef' x)        =
  (\(y, z) -> P.NewTypeDef' y : (P.DerivingDef' <$> z)) $ newtypeDef x
internalDef (H.DataDef' x)           =
  (\(y, z) -> P.DataDef' y : (P.DerivingDef' <$> z)) $ dataDef x
internalDef (H.FnDefOrSig' x)        = [P.FnDefOrSig' $ fnDefOrSig x]
internalDef (H.ClassDef' x)          = [P.ClassDef' $ classDef x]
internalDef (H.InstanceDef' x)       = [P.InstanceDef' $ instanceDef x]
internalDef (H.DerivingDef' x)       = [P.DerivingDef' $ derivingDef x]
internalDef (H.InfixFnAnnotation' x) = [P.InfixFnDef' $ infixFnAnnotation x]

moduleMember :: H.ModuleMember -> P.ModuleMember
moduleMember (H.VarMember x)  = P.VarMember $ var x
moduleMember (H.DataMember x) = P.DataMember $ typeVar x
