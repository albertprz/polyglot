module Parsers.Haskell.Pattern where


import Parsers.Haskell.Common ( token, ctor, var, literal, ctorOp )
import SyntaxTrees.Haskell.Common ()
import SyntaxTrees.Haskell.Pattern ( Pattern(..) )

import Parser ( Parser  )
import ParserCombinators
    ( anySepBy, manySepBy, (|?), (|+), (<|>), IsMatch(is), sepByOp)
import Parsers.String
    ( withinCurlyBrackets, maybeWithinParens, withinParens )
import Parsers.Char ( underscore, comma )
import Parsers.Collections ( listOf )


pattern' :: Parser Pattern
pattern' = maybeWithinParens pattern''
  where
  ctor'          = CtorPattern <$> ctor <*> (ctorElem' |+)
  nullaryCtor          = CtorPattern <$> ctor <*> pure []
  infixCtor = uncurry InfixCtorPattern <$> sepByOp ctorOp (ctor' <|> ctorElem')

  record         = RecordPattern <$> ctor <*> recordShape recordField
  recordWildcard = WildcardRecordPattern <$> ctor <*> recordShape
                                              (recordField <* (comma |?) <* is "..")

  alias    = AliasedPattern <$> (var <* is "@") <*> aliasElem'
  var'     = VarPattern  <$> var
  literal' = LitPattern  <$> literal
  wildcard = Wildcard    <$  token underscore

  list          = ListPattern  <$> listOf pattern'
  tuple         = TuplePattern <$> (withinParens $ manySepBy comma pattern')
  recordField   = (,)          <$> var <*> ((is "=" *> pattern'') |?)
  recordShape p = withinCurlyBrackets  (anySepBy comma p)


  elem'         = literal' <|> var' <|> wildcard <|> nullaryCtor <|>
                  tuple <|> list <|> withinParens alias

  ctorElem'     = alias <|> elem' <|>
                  withinParens ctor' <|> withinParens infixCtor <|>
                  record <|> recordWildcard

  aliasElem'    = elem' <|>
                  record <|> recordWildcard <|>
                  ctor' <|> infixCtor

  pattern''     = alias <|> infixCtor <|> ctor' <|> ctorElem'
