module Parsers.Haskell.Pattern where

import Parsers.Haskell.Common      (literal, qCtor, qCtorOp, token, var)
import SyntaxTrees.Haskell.Pattern (Pattern (..))

import Bookhound.Parser              (Parser, exactly)
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

    record = RecordPattern <$> qCtor <*> recordShape recordField
    recordWildcard = WildcardRecordPattern <$> qCtor
                                           <*> recordShape
                                           (recordField <* (comma |?) <* is "..")

    alias    = AliasedPattern <$> (var <* is "@") <*> aliasElem'
    var'     = VarPattern <$> var
    literal' = LitPattern <$> literal
    wildcard = Wildcard <$ token underscore

    list          = ListPattern <$> listOf pattern'
    tuple         = TuplePattern <$> (tupleOf pattern')
    recordField   = (,) <$> var <*> ((is "=" *> pattern'') |?)
    recordShape p = withinCurlyBrackets (anySepBy comma p)

    elem' =  literal' <|> var' <|> alias <|> wildcard <|> nullaryCtor <|>
            withinParens nullaryCtor <|>
            tuple <|> list

    ctorElem' = record <|> recordWildcard <|> alias <|> elem' <|>
                withinParens (ctor' <|> infixCtor)

    aliasElem' = exactly elem' <|> record <|> recordWildcard <|>
                 withinParens (ctor' <|> infixCtor)

    pattern'' = alias <|> infixCtor <|> ctor' <|> ctorElem'
