module Parsers.Haskell.Pattern where

import Parsers.Haskell.Common      (literal, qCtor, qCtorOp, token, var)
import Parsers.Haskell.Type        (type')
import SyntaxTrees.Haskell.Pattern (Pattern (..))

import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (IsMatch (is), anySepBy, sepByOp, (<|>),
                                      (|+), (|?))
import Bookhound.Parsers.Char        (comma, underscore)
import Bookhound.Parsers.Collections (listOf, tupleOf)
import Bookhound.Parsers.String      (maybeWithinParens, withinCurlyBrackets,
                                      withinParens)



pattern' :: Parser Pattern
pattern' =  pattern'' <|> maybeWithinParens pattern''
  where
    ctor'       = CtorPattern <$> qCtor <*> (ctorElem' |+)
    nullaryCtor = CtorPattern <$> qCtor <*> pure []
    infixCtor   = uncurry InfixCtorPattern <$> sepByOp qCtorOp (ctor' <|> ctorElem')

    record = RecordPattern <$> qCtor <*> recordShape
    recordWildcard = WildcardRecordPattern <$> qCtor
                                           <*> wildcardRecordShape

    alias    = AliasedPattern <$> (var <* is "@") <*> aliasElem'

    typeAnnotation = TypeAnnotation <$> ((ctor' <|> ctorElem') <* is "::")
                                    <*> type'
    var'     = VarPattern <$> var
    literal' = LitPattern <$> literal
    wildcard = Wildcard <$ token underscore

    list          = ListPattern <$> listOf pattern'
    tuple         = TuplePattern <$> (tupleOf pattern')
    recordField   = (,) <$> var <*> ((is "=" *> pattern'') |?)
    recordShape   = withinCurlyBrackets (anySepBy comma recordField)
    wildcardRecordShape =
      withinCurlyBrackets (anySepBy comma recordField <* token comma <* is "..")

    elem' = literal' <|> var' <|> alias <|> wildcard <|> nullaryCtor <|>
            withinParens nullaryCtor <|>
            tuple <|> list

    ctorElem' = recordPattern <|> alias <|> elem' <|>
                withinParens complexPattern

    aliasElem' = elem' <|> recordPattern <|>
                 withinParens complexPattern

    recordPattern = maybeWithinParens (record <|> recordWildcard)

    complexPattern = ctor' <|> infixCtor <|> typeAnnotation

    pattern'' = alias <|> infixCtor <|> ctor' <|> ctorElem'
