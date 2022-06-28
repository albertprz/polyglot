module Parsers.Haskell.ModuleDef where


import Parsers.Haskell.ClassDef      (classDef, instanceDef)
import Parsers.Haskell.Common        (module', token, var)
import Parsers.Haskell.DataDef       (dataDef, newtypeDef, typeDef)
import Parsers.Haskell.FnDef         (fnDef, fnSig, withinContextTupled)
import Parsers.Haskell.Type          (typeVar)
import SyntaxTrees.Haskell.ModuleDef (InternalDef (..), ModuleDef (..),
                                      ModuleExport (..), ModuleExportDef (..),
                                      ModuleImport (..), ModuleImportDef (..),
                                      ModuleMember (DataMember, VarMember))

import Data.Foldable             (Foldable (fold))
import Data.Maybe                (isJust)
import Parser                    (Parser)
import ParserCombinators         (IsMatch (is), anySepBy, maybeWithin, (<|>),
                                  (|?))
import Parsers.Char              (comma)
import Parsers.String            (spacing, withinParens)
import SyntaxTrees.Haskell.FnDef (FnDefOrSig (..))



moduleDef :: Parser ModuleDef
moduleDef = uncurry <$>
              (ModuleDef <$> (is "module" *> module')
                         <*> (moduleExport |?)
                         <*  is "where")
                         <*> withinContextTupled moduleImport internalDef



moduleExport :: Parser ModuleExport
moduleExport = ModuleExport <$> withinParens (anySepBy comma moduleExportDef)


moduleExportDef :: Parser ModuleExportDef
moduleExportDef = FnExport            <$> var                    <|>
                  DataExport          <$> typeVar                <|>
                  FullDataExport      <$> typeVar
                                      <*  withinParens (is "..") <|>
                  FilteredDataExport  <$> typeVar
                                      <*> withinParens (anySepBy comma moduleMember)

moduleImport :: Parser ModuleImport
moduleImport = ModuleImport <$> (token (is "import") *>
                                 (isJust <$> (is "qualified" |?)))
                            <*> module'
                            <*> ((is "as" *> module') |?)
                            <*> (fold <$> (defs |?))
  where
    defs = withinParens $ anySepBy comma $ maybeWithin spacing moduleImportDef


moduleImportDef :: Parser ModuleImportDef
moduleImportDef = FnImport            <$> var                    <|>
                  FullDataImport      <$> typeVar
                                      <*  withinParens (is "..") <|>
                  FilteredDataImport  <$> typeVar
                                      <*> withinParens (anySepBy comma moduleMember) <|>
                  DataImport          <$> typeVar



internalDef :: Parser InternalDef
internalDef = TypeDef'          <$> typeDef      <|>
              NewTypeDef'       <$> newtypeDef   <|>
              DataDef'          <$> dataDef      <|>
              FnDefOrSig' . Def <$> fnDef        <|>
              FnDefOrSig' . Sig <$> fnSig        <|>
              ClassDef'         <$> classDef     <|>
              InstanceDef'      <$> instanceDef



moduleMember :: Parser ModuleMember
moduleMember = VarMember  <$> var     <|>
              DataMember  <$> typeVar
