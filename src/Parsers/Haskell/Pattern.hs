module Parsers.Haskell.Pattern where

import Parser                      (Parser)
import ParserCombinators           (IsMatch (is), anySepBy, sepByOp, (<|>),
                                    (|+), (|?))
import Parsers.Char                (comma, underscore)
import Parsers.Collections         (listOf, tupleOf)
import Parsers.Haskell.Common      (literal, qCtor, qCtorOp, token, var)
import Parsers.String              (maybeWithinParens, withinCurlyBrackets,
                                    withinParens)
import SyntaxTrees.Haskell.Common  ()
import SyntaxTrees.Haskell.Pattern (Pattern (..))


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

    elem' = literal' <|> var' <|> wildcard <|> nullaryCtor <|>
            withinParens nullaryCtor <|>
            tuple <|> list <|> withinParens alias

    ctorElem' = record <|> recordWildcard <|> alias <|> elem' <|>
                withinParens ctor' <|> withinParens infixCtor

    aliasElem' = elem' <|> record <|> recordWildcard <|>
                 ctor' <|> infixCtor

    pattern'' = alias <|> infixCtor <|> ctor' <|> ctorElem'
