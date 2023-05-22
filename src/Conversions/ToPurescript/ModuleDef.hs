module Conversions.ToPurescript.ModuleDef where

import qualified SyntaxTrees.Haskell.ModuleDef    as H
import qualified SyntaxTrees.Haskell.Type         as H
import qualified SyntaxTrees.Purescript.Common    as P
import qualified SyntaxTrees.Purescript.ModuleDef as P

import Conversions.ToPurescript.ClassDef (classDef, derivingDef, instanceDef)
import Conversions.ToPurescript.Common   (module', var, varOp)
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
moduleExportDef (H.ModuleExportDef x)  = P.ModuleExportDef $ importExportDef x
moduleExportDef (H.FullModuleExport x) = P.FullModuleExport $ module' x

moduleImport :: H.ModuleImport -> P.ModuleImport
moduleImport (H.ModuleImport _ x y z t) =
  P.ModuleImport (module' x) z (moduleImportDef <$> t) (module' <$> y)


moduleImportDef :: H.ModuleImportDef -> P.ModuleImportDef
moduleImportDef (H.ModuleImportDef x)  = P.ModuleImportDef $ importExportDef x


importExportDef :: H.ImportExportDef -> P.ImportExportDef
importExportDef (H.Member x)       = P.Member $ moduleMember x
importExportDef (H.FullData x)     = P.FullData $ typeVar x
importExportDef (H.FilteredData (H.TypeVar name) y)
  | all isVarMember y = P.FullClass (P.Class name)
importExportDef (H.FilteredData x y) =
  P.FilteredData (typeVar x) (moduleMember <$> y)


isVarMember :: H.ModuleMember -> Bool
isVarMember (H.VarMember _)   = True
isVarMember (H.VarOpMember _) = True
isVarMember (H.DataMember _)  = False

moduleMember :: H.ModuleMember -> P.ModuleMember
moduleMember (H.VarMember x)   = P.VarMember $ var x
moduleMember (H.VarOpMember x) = P.VarOpMember $ varOp x
moduleMember (H.DataMember x)  = P.DataMember $ typeVar x

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
