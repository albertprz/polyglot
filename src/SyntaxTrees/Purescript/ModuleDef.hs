module SyntaxTrees.Purescript.ModuleDef where

import Data.Monoid.HT                  (when)
import SyntaxTrees.Purescript.ClassDef (ClassDef, DerivingDef, InstanceDef)
import SyntaxTrees.Purescript.Common   (Class, Module, Var, VarOp)
import SyntaxTrees.Purescript.DataDef  (DataDef, NewTypeDef, TypeDef)
import SyntaxTrees.Purescript.FnDef    (FnDefOrSig (Sig), InfixFnDef)
import SyntaxTrees.Purescript.Type     (TypeVar)
import Utils.String                    (joinMaybe, joinWords, wrapParens,
                                        wrapParensCsv, (+++))


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
  = ModuleExportDef ImportExportDef
  | FullModuleExport Module

data ModuleImport
  = ModuleImport
      { module'   :: Module
      , hiding    :: Bool
      , imporDefs :: [ModuleImportDef]
      , alias     :: Maybe Module
      }

data ModuleImportDef
  = ModuleImportDef ImportExportDef

data ImportExportDef
  = Member ModuleMember
  | FullData TypeVar
  | FilteredData TypeVar [ModuleMember]
  | FullClass Class

data ModuleMember
  = VarMember Var
  | VarOpMember VarOp
  | DataMember TypeVar

data InternalDef
  = TypeDef' TypeDef
  | NewTypeDef' NewTypeDef
  | DataDef' DataDef
  | FnDefOrSig' FnDefOrSig
  | ClassDef' ClassDef
  | InstanceDef' InstanceDef
  | DerivingDef' DerivingDef
  | InfixFnDef' InfixFnDef




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
  show (ModuleExportDef x)  = show x
  show (FullModuleExport x) = "module" +++ show x

instance Show ModuleImport where
  show (ModuleImport x y z t) =
    joinWords ["import",
               show x,
               when y "hiding",
               wrapParensCsv z,
               "as" `joinMaybe` t]

instance Show ModuleImportDef where
  show (ModuleImportDef x) = show x

instance Show ImportExportDef where
  show (Member x)         = show x
  show (FullData x)       = show x ++ wrapParens ".."
  show (FilteredData x y) = show x ++ wrapParensCsv y
  show (FullClass x)      = "class" +++ show x

instance Show ModuleMember where
  show (VarMember x)   = show x
  show (VarOpMember x) = wrapParens $ show x
  show (DataMember x)  = show x

instance Show InternalDef where
  show (TypeDef' x)     = show x
  show (NewTypeDef' x)  = show x
  show (DataDef' x)     = show x
  show (FnDefOrSig' x)  = show x
  show (ClassDef' x)    = show x
  show (InstanceDef' x) = show x
  show (DerivingDef' x) = show x
  show (InfixFnDef' x)  = show x


internalDefSeparator :: InternalDef -> String
internalDefSeparator (FnDefOrSig' (Sig _)) = "\n"
internalDefSeparator _                     = "\n\n"
