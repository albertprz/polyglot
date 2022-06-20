module Parsers.Haskell.Common where

import Data.Foldable              (Foldable (fold))
import Parser                     (Parser, check, withTransform)
import ParserCombinators          (IsMatch (inverse, is, isNot, noneOf, oneOf),
                                   maybeWithin, someSepBy, within, withinBoth,
                                   (<|>), (>>>), (|*), (|+), (|?))
import Parsers.Char               (alphaNum, char, colon, dot, lower, newLine,
                                   quote, underscore, upper)
import Parsers.Number             (double, int)
import Parsers.String             (spacing, withinDoubleQuotes, withinParens,
                                   withinQuotes)
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
var = Var <$> check "" (`notElem` reservedKeyWords)
                    (withinParens (operator opSymbol) <|> ident lower)

ctor :: Parser Ctor
ctor = Ctor <$> (ident upper <|> withinParens (operator colon))

varOp :: Parser VarOp
varOp = VarOp <$> check "" (`notElem` reservedSymbols)
                        (operator opSymbol <|> withinBackQuotes (ident lower))

ctorOp :: Parser CtorOp
ctorOp = CtorOp <$> (operator colon <|> withinBackQuotes (ident upper))

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
     ['!', '#', '$', '%', '&', '*', '+', '.', '/',
      '<', '=', '>', '?', '@', '\\', '|', '^', '|',
      '-', '~']


reservedKeyWords :: [String]
reservedKeyWords = ["case","class","data","default","deriving",
                    "do","else","forall" ,"if","import","in",
                    "infix","infixl","infixr","instance",
                    "let","module" ,"newtype","of","qualified",
                    "then","type","where","_" ,"foreign",
                    "ccall","as","safe","unsafe"]

reservedSymbols :: [String]
reservedSymbols = ["..","::","=","\\","|","<-","->","@","~","=>","[","]"]


withinBackQuotes :: Parser b -> Parser b
withinBackQuotes = within (is '`')
