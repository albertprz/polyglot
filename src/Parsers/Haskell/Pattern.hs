module Parsers.Haskell.Pattern where


import Parsers.Haskell.Common ( token, ctor, var, literal )
import SyntaxTrees.Haskell.Common ()
import SyntaxTrees.Haskell.Pattern ( Pattern(..) )

import Parser ( Parser )
import ParserCombinators
    ( anySeparatedBy, manySeparatedBy, (|?), (|*), (<|>), IsMatch(is) )
import Parsers.String
    ( withinCurlyBrackets, maybeWithinParens, withinParens )
import Parsers.Char ( underscore, comma )
import Parsers.Collections ( listOf )


pattern' :: Parser Pattern
pattern' = maybeWithinParens $ elem'
  where
  ctor'          = CtorPattern <$> ctor <*> ((ctorElem' <|> nullaryCtor) |*)
  nullaryCtor    = CtorPattern <$> ctor <*> pure []

  record         = RecordPattern <$> ctor <*> recordShape recordField
  recordWildcard = WildcardRecordPattern <$> ctor <*> recordShape
                                              (recordField <* (comma |?) <* is "..")

  alias    = AliasedPattern <$> (var <* is "@") <*> elem'
  var'     = VarPattern  <$> var
  literal' = LitPattern  <$> literal
  wildcard = Wildcard    <$  token underscore

  list          = ListPattern  <$> listOf pattern'
  tuple         = TuplePattern <$> (withinParens $ manySeparatedBy comma pattern')
  recordField   = (,)          <$> var <*> ((is "=" *> elem') |?)
  recordShape p = withinCurlyBrackets  (anySeparatedBy comma p)

  ctorElem'     = literal' <|> var' <|> tuple <|> list <|>
                  record <|> recordWildcard <|> alias <|>
                  withinParens ctor' <|> wildcard
  elem'         = ctorElem' <|> ctor'
