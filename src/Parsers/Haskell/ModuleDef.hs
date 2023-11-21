module Parsers.Haskell.ModuleDef where

import ClassyPrelude

import Parsers.Haskell.ClassDef (classDef, derivingDef, instanceDef)
import Parsers.Haskell.Common   (module', token, var, varOp)
import Parsers.Haskell.DataDef  (dataDef, newtypeDef, typeDef)
import Parsers.Haskell.FnDef    (betweenContextTupled, fnDef, fnSig,
                                 infixAnnotation)
import Parsers.Haskell.Type     (typeVar)

import SyntaxTrees.Haskell.FnDef     (FnDefOrSig (..))
import SyntaxTrees.Haskell.ModuleDef (ImportExportDef (..), InternalDef (..),
                                      ModuleDef (..), ModuleExport (..),
                                      ModuleExportDef (..), ModuleImport (..),
                                      ModuleImportDef (..), ModuleMember (..))

import Bookhound.Parser            (Parser)
import Bookhound.ParserCombinators (manySepBy, string, (|?))
import Bookhound.Parsers.Char      (comma)
import Bookhound.Parsers.Text      (betweenParens, maybeBetweenSpacing)



moduleDef :: Parser ModuleDef
moduleDef = uncurry <$>
              (ModuleDef <$> (string "module" *> module')
                         <*> (moduleExport |?)
                         <*  string "where")
                         <*> betweenContextTupled moduleImport internalDef



moduleExport :: Parser ModuleExport
moduleExport = ModuleExport <$> betweenParens (manySepBy comma moduleExportDef)


moduleExportDef :: Parser ModuleExportDef
moduleExportDef = ModuleExportDef  <$> importExportDef <|>
                  FullModuleExport <$> (string "module" *> module')

moduleImport :: Parser ModuleImport
moduleImport = ModuleImport <$> (token (string "import") *>
                                 (isJust <$> (string "qualified" |?)))
                            <*> module'
                            <*> ((string "as" *> module') |?)
                            <*> (isJust <$> (string "hiding" |?))
                            <*> (fold <$> (defs |?))
  where
    defs = betweenParens $ manySepBy comma $ maybeBetweenSpacing moduleImportDef


moduleImportDef :: Parser ModuleImportDef
moduleImportDef = ModuleImportDef <$> importExportDef


importExportDef :: Parser ImportExportDef
importExportDef = FullData     <$> typeVar
                               <*  betweenParens (string "..")      <|>
                  FilteredData <$> typeVar
                               <*> betweenParens
                                 (manySepBy comma moduleMember) <|>
                  Member       <$> moduleMember


moduleMember :: Parser ModuleMember
moduleMember = VarOpMember <$> betweenParens varOp <|>
               VarMember   <$> var                <|>
               DataMember  <$> typeVar

internalDef :: Parser InternalDef
internalDef = FnDefOrSig' . Def  <$> fnDef        <|>
              FnDefOrSig' . Sig  <$> fnSig        <|>
              TypeDef'           <$> typeDef      <|>
              NewTypeDef'        <$> newtypeDef   <|>
              DataDef'           <$> dataDef      <|>
              ClassDef'          <$> classDef     <|>
              InstanceDef'       <$> instanceDef  <|>
              DerivingDef'       <$> derivingDef  <|>
              InfixFnAnnotation' <$> infixAnnotation
