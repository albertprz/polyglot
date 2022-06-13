module Parsers.Haskell.Common where

import Parser                     (Parser, withTransform)
import ParserCombinators          (IsMatch (is, isNot, oneOf), maybeWithin,
                                   someSepBy, (<|>), (|*))
import Parsers.Char               (alphaNum, char, colon, dot, lower, quote,
                                   underscore, upper)
import Parsers.Number             (double, int)
import Parsers.String             (spacing, withinDoubleQuotes, withinQuotes)
import SyntaxTrees.Haskell.Common (Class (..), Ctor (..), CtorOp (..),
                                   Literal (..), Module (..), Var (..),
                                   VarOp (..))

literal :: Parser Literal
literal = UnitLit <$ is "()" <|>
          BoolLit <$> (True <$ is "True" <|> False <$ is "False") <|>
          IntLit . show <$> int <|>
          FloatLit . show <$> double <|>
          CharLit . pure <$> withinQuotes char <|>
          StringLit <$> withinDoubleQuotes (isNot '"' |*)

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
module' = Module <$> someSepBy dot (ident upper)

ident :: Parser Char -> Parser String
ident start = token $ (:) <$> start <*> (idChar |*)

operator :: Parser Char -> Parser String
operator start = token $ (:) <$> start
                             <*> ((opSymbol <|> colon) |*)

idChar :: Parser Char
idChar = alphaNum <|> underscore <|> quote

opSymbol :: Parser Char
opSymbol = oneOf
    [ '!', '#', '$', '%', '&', '⋆', '+', '.', '/',
      '<', '=', '>', '?', '@', '\\', '|', '^', '|',
      '-', '~']

token :: Parser a -> Parser a
token = withTransform $ maybeWithin spacing
