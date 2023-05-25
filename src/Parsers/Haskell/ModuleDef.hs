module Parsers.Haskell.ModuleDef where


import Parsers.Haskell.ClassDef (classDef, derivingDef, instanceDef)
import Parsers.Haskell.Common   (module', token, var, varOp)
import Parsers.Haskell.DataDef  (dataDef, newtypeDef, typeDef)
import Parsers.Haskell.FnDef    (fnDef, fnSig, infixAnnotation,
                                 withinContextTupled)
import Parsers.Haskell.Type     (typeVar)

import SyntaxTrees.Haskell.FnDef     (FnDefOrSig (..))
import SyntaxTrees.Haskell.ModuleDef (ImportExportDef (..), InternalDef (..),
                                      ModuleDef (..), ModuleExport (..),
                                      ModuleExportDef (..), ModuleImport (..),
                                      ModuleImportDef (..), ModuleMember (..))

import Bookhound.Parser            (Parser)
import Bookhound.ParserCombinators (IsMatch (is), anySepBy, maybeWithin, (<|>),
                                    (|?))
import Bookhound.Parsers.Char      (comma)
import Bookhound.Parsers.String    (spacing, withinParens)


import Data.Foldable (Foldable (fold))
import Data.Maybe    (isJust)



moduleDef :: Parser ModuleDef
moduleDef = uncurry <$>
              (ModuleDef <$> (is "module" *> module')
                         <*> (moduleExport |?)
                         <*  is "where")
                         <*> withinContextTupled moduleImport internalDef



moduleExport :: Parser ModuleExport
moduleExport = ModuleExport <$> withinParens (anySepBy comma moduleExportDef)


moduleExportDef :: Parser ModuleExportDef
moduleExportDef = ModuleExportDef  <$> importExportDef <|>
                  FullModuleExport <$> (is "module" *> module')

moduleImport :: Parser ModuleImport
moduleImport = ModuleImport <$> (token (is "import") *>
                                 (isJust <$> (is "qualified" |?)))
                            <*> module'
                            <*> ((is "as" *> module') |?)
                            <*> (isJust <$> (is "hiding" |?))
                            <*> (fold <$> (defs |?))
  where
    defs = withinParens $ anySepBy comma $ maybeWithin spacing moduleImportDef


moduleImportDef :: Parser ModuleImportDef
moduleImportDef = ModuleImportDef <$> importExportDef


importExportDef :: Parser ImportExportDef
importExportDef = FullData     <$> typeVar
                               <*  withinParens (is "..")      <|>
                  FilteredData <$> typeVar
                               <*> withinParens
                                 (anySepBy comma moduleMember) <|>
                  Member       <$> moduleMember


moduleMember :: Parser ModuleMember
moduleMember = VarOpMember <$> withinParens varOp <|>
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
