module Parsers.Haskell.DataDef  where

import Parser (Parser)
import Parsers.String ( withinCurlyBrackets )

import ParserCombinators
    ( anySepBy, someSepBy,
      (|?), (|*), (<|>), (<#>), IsMatch(..) )

import SyntaxTrees.Haskell.Common ( Class )

import Parsers.Haskell.Type
    ( typeParam, typeVar, anyKindedType, type' )

import SyntaxTrees.Haskell.DataDef
    ( DataCtorDef(..), DataDef(..), FieldDef(..), NamedFieldDef(..),
      NewTypeDef(..), TypeDef(..), UnNamedFieldDef(..) )


import Parsers.Haskell.Common ( class', ctor, var )
import Parsers.Char ( comma, colon, equal )
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
    alternatives = ((equal *> someSepBy (is "|") dataCtorDef) |?)


namedFieldDef :: Parser NamedFieldDef
namedFieldDef = NamedFieldDef <$> var <* (colon <#> 2) <*> type'


unNamedFieldDef :: Parser UnNamedFieldDef
unNamedFieldDef = UnNamedFieldDef <$> type'


fieldDef :: Parser FieldDef
fieldDef = UnNamedField <$> unNamedFieldDef <|>
           NamedField   <$> withinCurlyBrackets namedFieldDef

dataCtorDef :: Parser DataCtorDef
dataCtorDef = NamedFieldsCtor   <$> ctor
                                <*> (withinCurlyBrackets $
                                     anySepBy comma namedFieldDef) <|>
              UnNamedFieldsCtor <$> ctor
                                <*> (unNamedFieldDef |*)

derivingList :: Parser [Class]
derivingList = mconcat . toList <$>
                  ((is "deriving" *> anySepBy comma class') |?)
