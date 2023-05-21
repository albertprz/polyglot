module SyntaxTrees.Purescript.ModuleDef where

import Data.Monoid.HT                  (when)
import SyntaxTrees.Purescript.ClassDef (ClassDef, DerivingDef, InstanceDef)
import SyntaxTrees.Purescript.Common   (Module, Var)
import SyntaxTrees.Purescript.DataDef  (DataDef, NewTypeDef, TypeDef)
import SyntaxTrees.Purescript.FnDef    (FnDefOrSig (Sig), InfixFnDef)
import SyntaxTrees.Purescript.Type     (TypeVar)
import Utils.String                    (joinMaybe, joinWords, wrapParens,
                                        wrapParensCsv)


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
  = FnExport Var
  | DataExport TypeVar
  | FullDataExport TypeVar
  | FilteredDataExport TypeVar [ModuleMember]

data ModuleImport
  = ModuleImport Module (Maybe Module) Bool [ModuleImportDef]

data ModuleImportDef
  = FnImport Var
  | DataImport TypeVar
  | FullDataImport TypeVar
  | FilteredDataImport TypeVar [ModuleMember]

data InternalDef
  = TypeDef' TypeDef
  | NewTypeDef' NewTypeDef
  | DataDef' DataDef
  | FnDefOrSig' FnDefOrSig
  | ClassDef' ClassDef
  | InstanceDef' InstanceDef
  | DerivingDef' DerivingDef
  | InfixFnDef' InfixFnDef

data ModuleMember
  = VarMember Var
  | DataMember TypeVar



instance Show ModuleDef where
  show (ModuleDef x y z t) =
    joinWords ["module",
               show x,
               foldMap show y,
               "where",
               "\n\n" ++ unlines (show <$> z),
               "\n\n" ++ foldMap (\l -> show l ++ internalDefSeparator l) t]

instance Show ModuleExport where
  show (ModuleExport x) = wrapParensCsv x

instance Show ModuleExportDef where
  show (FnExport x)             = show x
  show (DataExport x)           = show x
  show (FullDataExport x)       = show x ++ wrapParens ".."
  show (FilteredDataExport x y) = show x ++ wrapParensCsv y

instance Show ModuleImport where
  show (ModuleImport x y z t) =
    joinWords ["import",
               show x,
               "as" `joinMaybe` y,
               when z "hiding",
               wrapParensCsv t]

instance Show ModuleImportDef where
  show (FnImport x)             = show x
  show (DataImport x)           = show x
  show (FullDataImport x)       = show x ++ wrapParens ".."
  show (FilteredDataImport x y) = show x ++ wrapParensCsv y

instance Show InternalDef where
  show (TypeDef' x)     = show x
  show (NewTypeDef' x)  = show x
  show (DataDef' x)     = show x
  show (FnDefOrSig' x)  = show x
  show (ClassDef' x)    = show x
  show (InstanceDef' x) = show x
  show (DerivingDef' x) = show x
  show (InfixFnDef' x)  = show x

instance Show ModuleMember where
  show (VarMember x)  = show x
  show (DataMember x) = show x


internalDefSeparator :: InternalDef -> String
internalDefSeparator (FnDefOrSig' (Sig _)) = "\n"
internalDefSeparator _                     =  "\n\n"
