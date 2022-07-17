module Parsers.Haskell.DataDef where

import Parsers.Haskell.Common      (class', ctor, var)
import Parsers.Haskell.Type        (anyKindedType, type', typeParam, typeVar)
import SyntaxTrees.Haskell.Common  (Class)
import SyntaxTrees.Haskell.DataDef (DataCtorDef (..), DataDef (..),
                                    FieldDef (..), NamedFieldDef (..),
                                    NewTypeDef (..), TypeDef (..),
                                    UnNamedFieldDef (..))

import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (IsMatch (..), anySepBy, someSepBy, (<#>),
                                      (<|>), (|*), (|?))
import Bookhound.Parsers.Char        (colon, comma, equal)
import Bookhound.Parsers.Collections (tupleOf)
import Bookhound.Parsers.String      (maybeWithinParens, withinCurlyBrackets,
                                      withinParens)

import Data.Foldable (Foldable (fold))


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
                  <*> (fold <$> alternatives)
                  <*> derivingList
  where
    alternatives = ((equal *> someSepBy (is "|") dataCtorDef) |?)

namedFieldDef :: Parser NamedFieldDef
namedFieldDef = NamedFieldDef <$> var <* (colon <#> 2)
                              <*> type'

unNamedFieldDef :: Parser UnNamedFieldDef
unNamedFieldDef = UnNamedFieldDef <$> (withinParens type' <|> type')

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
derivingList = fold <$> ((is "deriving" *> (tupleOf class' <|>
                                            pure <$> maybeWithinParens class')) |?)
