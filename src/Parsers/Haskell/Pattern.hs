module Parsers.Haskell.Pattern where

import Parsers.Haskell.Common      (literal, qCtor, qCtorOp, token, var)
import Parsers.Haskell.Type        (type')
import SyntaxTrees.Haskell.Pattern (Pattern (..))

import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (char, manySepBy, sepByOp, string, (|+),
                                      (|?))
import Bookhound.Parsers.Char        (comma, underscore)
import Bookhound.Parsers.Collections (listOf, tupleOf)
import Bookhound.Parsers.Text        (betweenCurlyBrackets, betweenParens,
                                      maybeBetweenParens)
import Control.Applicative           ((<|>))



pattern' :: Parser Pattern
pattern' =  pattern'' <|> maybeBetweenParens pattern''
  where
    ctor'       = CtorPattern <$> qCtor <*> (ctorElem' |+)
    nullaryCtor = CtorPattern <$> qCtor <*> pure []
    infixCtor   = uncurry InfixCtorPattern <$> sepByOp qCtorOp (ctor' <|> ctorElem')

    record = RecordPattern <$> qCtor <*> recordShape
    recordWildcard = WildcardRecordPattern <$> qCtor
                                           <*> wildcardRecordShape

    alias    = AliasedPattern <$> (var <* char '@') <*> aliasElem'

    typeAnnotation = TypeAnnotation <$> ((ctor' <|> ctorElem') <* string "::")
                                    <*> type'
    var'     = VarPattern <$> var
    literal' = LitPattern <$> literal
    wildcard = Wildcard <$ token underscore

    list          = ListPattern <$> listOf pattern'
    tuple         = TuplePattern <$> tupleOf pattern'
    recordField   = (,) <$> var <*> ((char '=' *> pattern'') |?)
    recordShape   = betweenCurlyBrackets (manySepBy comma recordField)
    wildcardRecordShape =
      betweenCurlyBrackets (manySepBy comma recordField <* token comma <* string "..")

    elem' = literal' <|> var' <|> alias <|> wildcard <|> nullaryCtor <|>
            betweenParens nullaryCtor <|>
            tuple <|> list

    ctorElem' = recordPattern <|> alias <|> elem' <|>
                betweenParens complexPattern

    aliasElem' = elem' <|> recordPattern <|>
                 betweenParens complexPattern

    recordPattern = maybeBetweenParens (record <|> recordWildcard)

    complexPattern = ctor' <|> infixCtor <|> typeAnnotation

    pattern'' = alias <|> infixCtor <|> ctor' <|> ctorElem'
