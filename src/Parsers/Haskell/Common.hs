module Parsers.Haskell.Common  where

import SyntaxTrees.Haskell.Common
    ( Module(..), Class(..), CtorOp(..), VarOp(..),
      Ctor(..), Var(..), Literal(..) )

import Parser (Parser, withTransform)
import ParserCombinators ( (<|>), (|*), IsMatch (oneOf, is, isNot),
                           maybeWithin)
import Parsers.String ( withinDoubleQuotes, withinQuotes, spacing)
import Parsers.Char
    ( quote, alphaNum, underscore, dot, colon, upper, lower, char )

import Parsers.Number ( double, int )


literal :: Parser Literal
literal = UnitLit          <$  is "()"           <|>
          IntLit . show    <$> int               <|>
          FloatLit . show  <$> double            <|>
          CharLit . (: []) <$> withinQuotes char <|>
          StringLit        <$> withinDoubleQuotes (isNot '"' |*)

var :: Parser Var
var = Var <$> ident lower

ctor :: Parser Ctor
ctor = Ctor <$> ident upper

varOp :: Parser VarOp
varOp = VarOp <$> operator opSymbol

ctorOp :: Parser CtorOp
ctorOp = CtorOp <$> operator colon

class' :: Parser Class
class' = Class <$> ident upper

module' :: Parser Module
module' = Module <$> ((:) <$> ident upper <*> ((dot *> ident upper) |*))



ident :: Parser Char -> Parser String
ident start = token $ (:) <$> start <*> (idChar |*)

operator :: Parser Char -> Parser String
operator start = token $ (:) <$> start <*> ((opSymbol <|> colon) |*)

idChar :: Parser Char
idChar = alphaNum <|> underscore <|> quote

opSymbol :: Parser Char
opSymbol = oneOf ['!', '#', '$', '%', '&', '⋆', '+', '.', '/', '<', '=',
                '>', '?', '@', '\\', '|', '^', '|', '-', '~']

token :: Parser a -> Parser a
token = withTransform $ maybeWithin spacing
