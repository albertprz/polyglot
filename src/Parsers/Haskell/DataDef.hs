module Parsers.Haskell.DataDef  where

import Parser (Parser, runParser)
import Parsers.String

import ParserCombinators

import SyntaxTrees.Haskell.Common


import SyntaxTrees.Haskell.DataDef


import Parsers.Haskell.Common
import Parsers.Char
import Data.Foldable(toList)


typeDef :: Parser TypeDef
typeDef = TypeDef <$> ((is "type") *> typeVar)
                  <*> (typeParam |*) <* equal
                  <*> anyKindedType

newtypeDef :: Parser NewTypeDef
newtypeDef = NewTypeDef <$> (is "newtype" *> typeVar)
                        <*> (typeParam |*) <* equal
                        <*> ctor
                        <*> fieldDef
                        <*> derivingList

dataDef :: Parser DataDef
dataDef = DataDef <$> (is "data" *> typeVar)
                  <*> (typeParam |*)
                  <*> (mconcat . toList <$> alternatives)
                  <*> derivingList
  where
    alternatives = ((equal *> someSeparatedBy (is "|") dataCtorDef) |?)


namedFieldDef :: Parser NamedFieldDef
namedFieldDef = NamedFieldDef <$> var <* (colon <#> 2) <*> type'


unNamedFieldDef :: Parser UnNamedFieldDef
unNamedFieldDef = UnNamedFieldDef <$> type'


fieldDef :: Parser FieldDef
fieldDef = UnNamedField <$> unNamedFieldDef <|>
           NamedField   <$> withinCurlyBrackets namedFieldDef

dataCtorDef :: Parser DataCtorDef
dataCtorDef = NamedFieldsCtor   <$> ctor
                                <*> (withinCurlyBrackets $ anySeparatedBy comma namedFieldDef) <|>
              UnNamedFieldsCtor <$> ctor
                                <*> (unNamedFieldDef |*)

derivingList :: Parser [Class]
derivingList = mconcat . toList <$>
                  ((is "deriving" *> anySeparatedBy comma class') |?)
