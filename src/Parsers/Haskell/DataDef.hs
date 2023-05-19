module Parsers.Haskell.DataDef where

import Parsers.Haskell.Common      (class', ctor, var)
import Parsers.Haskell.Type        (anyKindedType, type', typeCtor, typeParam)
import SyntaxTrees.Haskell.DataDef (DataCtorDef (..), DataDef (..),
                                    DerivingClause (..), FieldDef (..),
                                    NamedFieldDef (..), NewTypeDef (..),
                                    TypeDef (..), UnNamedFieldDef (..))

import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (IsMatch (..), anySepBy, someSepBy, (<#>),
                                      (<|>), (|*), (|?))
import Bookhound.Parsers.Char        (colon, comma, equal)
import Bookhound.Parsers.Collections (tupleOf)
import Bookhound.Parsers.String      (maybeWithinParens, withinCurlyBrackets,
                                      withinParens)

import Data.Foldable (Foldable (fold))


typeDef :: Parser TypeDef
typeDef = TypeDef <$> ((is "type") *> typeCtor)
                  <*> (typeParam |*) <* equal
                  <*> anyKindedType

newtypeDef :: Parser NewTypeDef
newtypeDef = NewTypeDef <$> (is "newtype" *> typeCtor)
                        <*> (typeParam |*) <* equal
                        <*> ctor
                        <*> fieldDef
                        <*> (derivingClause |*)

dataDef :: Parser DataDef
dataDef = DataDef <$> (is "data" *> typeCtor)
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
