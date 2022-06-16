module Parsers.Haskell.Common where

import Data.Foldable              (Foldable (fold))
import Parser                     (Parser, withTransform)
import ParserCombinators          (IsMatch (inverse, is, isNot, noneOf, oneOf),
                                   maybeWithin, someSepBy, withinBoth, (<|>),
                                   (>>>), (|*), (|+), (|?))
import Parsers.Char               (alphaNum, char, colon, dot, lower, newLine,
                                   quote, underscore, upper)
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
opSymbol = oneOf symbolChars

token :: Parser a -> Parser a
token = withTransform $ maybeWithin (anyComment |+) . maybeWithin spacing


anyComment :: Parser String
anyComment = lineComment <|> blockComment <|> pragma

lineComment :: Parser String
lineComment = is "--" *> (pure <$> newLine <|>
                          noneOf symbolChars >>> (inverse newLine |?))


blockComment :: Parser String
blockComment = fold <$> withinBoth (is "{-") (is "-}") (isNot "#" *>
                                   ((isNot "-" *> isNot "}") |*))


pragma :: Parser String
pragma = fold <$> withinBoth (is "{-#") (is "#-}")
                             (((isNot "#" *> isNot "-" *> isNot "}") |*))


symbolChars :: [Char]
symbolChars =
     ['!', '#', '$', '%', '&', '⋆', '+', '.', '/',
      '<', '=', '>', '?', '@', '\\', '|', '^', '|',
      '-', '~']
