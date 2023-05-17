module Parsers.Haskell.DataDef where

import Parsers.Haskell.Common      (class', ctor, var)
import Parsers.Haskell.Type        (anyKindedType, type', typeParam, typeVar)
import SyntaxTrees.Haskell.DataDef (DataCtorDef (..), DataDef (..),
                                    FieldDef (..), NamedFieldDef (..),
                                    NewTypeDef (..), TypeDef (..),
                                    UnNamedFieldDef (..), DerivingClause (..))

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
                        <*> (derivingClause |*)

dataDef :: Parser DataDef
dataDef = DataDef <$> (is "data" *> typeVar)
                  <*> (typeParam |*)
                  <*> (fold <$> alternatives)
                  <*> (derivingClause |*)
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

derivingClause :: Parser DerivingClause
derivingClause = (is "deriving" *>
                  ((is "stock" |?) *>
                      (StandardDeriving <$> derivingList))
                  <|> (is "newtype" *>
                       (NewTypeDeriving <$> derivingList))
                  <|> (is "anyclass" *>
                       (AnyClassDeriving <$> derivingList))
                  <|> (DerivingVia <$> derivingList
                                   <*> (is "via" *> anyKindedType)))
  where
    derivingList = (tupleOf class' <|> pure <$> maybeWithinParens class')
