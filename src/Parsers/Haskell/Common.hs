module Parsers.Haskell.Common where

import Data.Foldable (Foldable (fold))


import Parser                     (Parser, check, withTransform)
import ParserCombinators          (IsMatch (inverse, is, isNot, noneOf, oneOf),
                                   maybeWithin, someSepBy, within, withinBoth,
                                   (<|>), (>>>), (|*), (|+), (|?))
import Parsers.Char               (alpha, alphaNum, char, colon, dot, lower,
                                   newLine, quote, underscore, upper)
import Parsers.Number             (double, int)
import Parsers.String             (spacing, withinDoubleQuotes, withinParens,
                                   withinQuotes)
import SyntaxTrees.Haskell.Common (Class (..), Ctor (..), CtorOp (..),
                                   Literal (..), Module (..), QClass (QClass),
                                   QCtor (..), QCtorOp (..), QVar (..),
                                   QVarOp (..), Var (..), VarOp (..))
import Utils.Foldable             (wrapMaybe)
import Utils.String               (wrap, wrapBackQuotes, wrapQuotes)



literal :: Parser Literal
literal = token $
      UnitLit <$ is "()"
  <|> BoolLit <$> (True <$ is "True" <|> False <$ is "False")
  <|> IntLit   . show <$> int
  <|> FloatLit . show <$> double
  <|> CharLit   <$> withinQuotes        (charLit   <|> charLitEscaped)
  <|> StringLit <$> withinDoubleQuotes ((stringLit <|> charLitEscaped) |*)

  where
    charLit = noneOf ['\'', '\\']
    charLitEscaped = read . wrapQuotes <$> (is '\\' >>> alpha)
                     <|> (is '\\' *> char)
    stringLit = noneOf ['"', '\\']



var :: Parser Var
var = Var <$> notReserved
              (withinParens (operator opSymbol <|> simpleOperator <|> simpleOperatorFn)
               <|> ident lower)

ctor :: Parser Ctor
ctor = Ctor <$> notReserved
                (withinParens (operator colon) <|> ident upper)

varOp :: Parser VarOp
varOp = VarOp <$> notReserved
                 (wrapBackQuotes <$> withinBackQuotes (ident lower)
                  <|> (operator opSymbol <|> simpleOperator))

ctorOp :: Parser CtorOp
ctorOp = CtorOp <$> notReserved
                    (wrapBackQuotes <$> withinBackQuotes (ident upper)
                     <|> operator colon)

class' :: Parser Class
class' = Class <$> ident upper

module' :: Parser Module
module' = Module <$> someSepBy dot (ident upper)

module'' :: Parser Module
module'' = Module <$> someSepBy dot (nonTokenIdent upper)

qVar :: Parser QVar
qVar = uncurry QVar <$> qTerm var

qCtor :: Parser QCtor
qCtor = uncurry QCtor <$> qTerm' Ctor

qVarOp :: Parser QVarOp
qVarOp = uncurry QVarOp <$> qTerm varOp

qCtorOp :: Parser QCtorOp
qCtorOp = uncurry QCtorOp <$> qTerm ctorOp

qClass :: Parser QClass
qClass = uncurry QClass <$> qTerm' Class



ident :: Parser Char -> Parser String
ident start = token $ (:) <$> start <*> (idChar |*)

operator :: Parser Char -> Parser String
operator start = token $ (:) <$> start
                             <*> ((opSymbol <|> colon) |*)

simpleOperator :: Parser String
simpleOperator = token $ oneOf [":"]

simpleOperatorFn :: Parser String
simpleOperatorFn = token $ oneOf [",", ",,", ",,,"]

nonTokenQVar :: Parser QVar
nonTokenQVar = uncurry QVar <$> qTerm x
  where  x = Var <$> check "" (`notElem` reservedKeyWords)
                    (nonTokenIdent lower)

nonTokenIdent :: Parser Char -> Parser String
nonTokenIdent start = (:) <$> start <*> (idChar |*)


idChar :: Parser Char
idChar = alphaNum <|> underscore <|> quote

opSymbol :: Parser Char
opSymbol = oneOf symbolChars

token :: Parser a -> Parser a
token = withTransform $ maybeWithin (anyComment |+) . maybeWithin spacing


qTerm :: Parser a -> Parser (Maybe Module, a)
qTerm x =  (,) <$> ((module'' <* dot) |?) <*> x


qTerm' :: (String -> b) -> Parser (Maybe Module, b)
qTerm' fn = token parser
  where
    parser = do xs <- getComponents <$> module''
                pure $ (Module <$> wrapMaybe (init xs), fn $ last xs)
    getComponents (Module xs) = xs


anyComment :: Parser String
anyComment = pragma <|> blockComment <|> lineComment

lineComment :: Parser String
lineComment = is "--" >>> (pure <$> newLine <|>
                          noneOf symbolChars >>> (inverse newLine |*))


blockComment :: Parser String
blockComment = wrap "{-"  "-}" . fold <$> withinBoth (is "{-") (is "-}")
                         ((:) <$> (isNot "#") <*> ((isNot "-" >>> isNot "}") |*))


pragma :: Parser String
pragma = wrap "{-#"  "#-}" . fold <$> withinBoth (is "{-#") (is "#-}")
                             ((isNot "#" |*))

symbolChars :: [Char]
symbolChars =
     ['!', '#', '$', '%', '&', '*', '+', '.', '/',
      '<', '=', '>', '?', '@', '\\', '|', '^', '|',
      '-', '~']


notReserved :: Parser String -> Parser String
notReserved = check "reserved"
               (`notElem` (reservedSymbols ++ reservedKeyWords))


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
