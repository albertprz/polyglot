module Parsers.Haskell.DataDef where

import Parsers.Haskell.Common      (class', ctor, var)
import Parsers.Haskell.Type        (anyKindedType, standaloneType, type',
                                    typeCtor, typeParam)
import SyntaxTrees.Haskell.DataDef (DataCtorDef (..), DataDef (..),
                                    DerivingClause (..), DerivingStrategy (..),
                                    FieldDef (..), NamedFieldDef (..),
                                    NewTypeDef (..), TypeDef (..),
                                    UnNamedFieldDef (..))

import Bookhound.Parser              (Parser, withError)
import Bookhound.ParserCombinators   (char, manySepBy, someSepBy, string, (<#>),
                                      (|*), (|?))
import Bookhound.Parsers.Char        (colon, comma, equal)
import Bookhound.Parsers.Collections (tupleOf)
import Bookhound.Parsers.Text        (betweenCurlyBrackets, maybeBetweenParens)

import Control.Applicative ((<|>))
import Data.Foldable       (Foldable (fold))


typeDef :: Parser TypeDef
typeDef = withError "Type declaration" $
  TypeDef <$> (string "type" *> typeCtor)
          <*> (typeParam |*) <* equal
          <*> anyKindedType

newtypeDef :: Parser NewTypeDef
newtypeDef = withError "Newtype declaration" $
  NewTypeDef <$> (string "newtype" *> typeCtor)
             <*> (typeParam |*) <* equal
             <*> ctor
             <*> fieldDef
             <*> (derivingClause |*)

dataDef :: Parser DataDef
dataDef = withError "Data declaration" $
  DataDef <$> (string "data" *> typeCtor)
          <*> (typeParam |*)
          <*> (fold <$> alternatives)
          <*> (derivingClause |*)
  where
    alternatives = ((equal *> someSepBy (char '|') dataCtorDef) |?)


namedFieldDef :: Parser NamedFieldDef
namedFieldDef = NamedFieldDef <$> var <* (colon <#> 2)
                              <*> type'

unNamedFieldDef :: Parser UnNamedFieldDef
unNamedFieldDef = UnNamedFieldDef <$> standaloneType

fieldDef :: Parser FieldDef
fieldDef = UnNamedField <$> unNamedFieldDef <|>
           NamedField   <$> betweenCurlyBrackets namedFieldDef

dataCtorDef :: Parser DataCtorDef
dataCtorDef = NamedFieldsCtor   <$> ctor
                                <*> betweenCurlyBrackets
                                   (manySepBy comma namedFieldDef)
              <|>
              UnNamedFieldsCtor <$> ctor
                                <*> (unNamedFieldDef |*)

derivingClause :: Parser DerivingClause
derivingClause = string "deriving" *>
                  ((string "stock" |?) *>
                      (Deriving StandardDeriving <$> derivingList))
                  <|> (string "newtype" *>
                       (Deriving NewTypeDeriving <$> derivingList))
                  <|> (string "anyclass" *>
                       (Deriving AnyClassDeriving <$> derivingList))
                  <|> (DerivingVia <$> derivingList
                                   <*> (string "via" *> anyKindedType))
  where
    derivingList = tupleOf class' <|> pure <$> maybeBetweenParens class'
