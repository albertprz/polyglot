module Parsers.Haskell.ModuleDef where

import           Parser                    (Parser)
import           ParserCombinators         (IsMatch (is), sepByOp, someSepBy,
                                            (<|>), (|*), (|?), anySepBy)

import SyntaxTrees.Haskell.ModuleDef
import Parsers.Haskell.DataDef (typeDef, newtypeDef, dataDef)
import Parsers.Haskell.FnDef (fnDef, fnSig, withinContext)
import Parsers.Haskell.ClassDef (classDef, instanceDef)
import Parsers.Haskell.Common (var, module')
import Parsers.Haskell.Type (typeVar)
import Parsers.String (withinParens)
import Parsers.Char (comma)


moduleDef = ModuleDef <$> (is "module" *> module')
                      <*> (moduleExport |?)
                      <*> (moduleImport |*)
                      <*> withinContext internalDef



moduleExport = ModuleExport <$> withinParens (anySepBy comma moduleExportDef)


moduleExportDef = FnExport            <$> var                   <|>
                  DataExport          <$> typeVar               <|>
                  FullDataExport      <$> typeVar
                                      <* withinParens (is "..") <|>
                  FilteredDataExport  <$> typeVar
                                      <*> withinParens (anySepBy comma var)

moduleImport = ModuleImport <$> (is "import" *> module')
                            <*> withinParens (anySepBy comma moduleImportDef)


moduleImportDef = FnImport            <$> var                   <|>
                  DataImport          <$> typeVar               <|>
                  FullDataImport      <$> typeVar
                                      <* withinParens (is "..") <|>
                  FilteredDataImport  <$> typeVar
                                      <*> withinParens (anySepBy comma var)



internalDef :: Parser InternalDef
internalDef = TypeDef'     <$> typeDef      <|>
              NewTypeDef'  <$> newtypeDef   <|>
              DataDef'     <$> dataDef      <|>
              FnDef'       <$> fnDef        <|>
              FnSig'       <$> fnSig        <|>
              ClassDef'    <$> classDef     <|>
              InstanceDef' <$> instanceDef
