module Parsers.Haskell.Common where

import ClassyPrelude


import           Bookhound.Parser            (Parser, anyChar, satisfy,
                                              withTransform)
import           Bookhound.ParserCombinators (IsMatch (inverse, is, isNot, noneOf, oneOf),
                                              between, maybeSurroundedBy,
                                              someSepBy, string, surroundedBy,
                                              (->>-), (<:>), (|*), (|+), (|?),
                                              (||*))
import           Bookhound.Parsers.Char      (alpha, alphaNum, colon, dash, dot,
                                              lower, newLine, quote, underscore,
                                              upper)
import           Bookhound.Parsers.Number    (double, int, negInt)
import           Bookhound.Parsers.Text      (betweenDoubleQuotes,
                                              betweenParens, betweenQuotes,
                                              maybeBetweenSpacing)
import qualified Data.Text                   as Text
import           SyntaxTrees.Haskell.Common  (Class (..), Ctor (..),
                                              CtorOp (..), Literal (..),
                                              Module (..), QClass (QClass),
                                              QCtor (..), QCtorOp (..),
                                              QVar (..), QVarOp (..), Var (..),
                                              VarOp (..))
import           Text.Read                   (read)
import           Utils.Foldable              (wrapMaybe)
import           Utils.List                  (initList)
import           Utils.String                (overText, wrap, wrapBackQuotes,
                                              wrapQuotes)



literal :: Parser Literal
literal = token $
      UnitLit <$ string "()"
  <|> BoolLit <$> (True <$ string "True" <|> False <$ string "False")
  <|> IntLit   . tshow <$> int
  <|> IntLit   . tshow <$> betweenParens negInt
  <|> FloatLit . tshow <$> double
  <|> FloatLit . tshow <$> betweenParens (dash ->>- double)
  <|> CharLit   <$> betweenQuotes        (charLit   <|> charLitEscaped)
  <|> StringLit <$> betweenDoubleQuotes ((stringLit <|> charLitEscaped) ||*)

  where
    charLit = noneOf ['\'', '\\']
    charLitEscaped = read . wrapQuotes . unpack <$> (is '\\' ->>- alpha)
                     <|> (is '\\' *> anyChar)
    stringLit = noneOf ['"', '\\']



var :: Parser Var
var = Var <$> notReserved
              (betweenParens (operator opSymbol <|> simpleOperator <|> simpleOperatorFn)
               <|> ident lower)

ctor :: Parser Ctor
ctor = Ctor <$> notReserved
                (betweenParens (operator colon) <|> ident upper)

varOp :: Parser VarOp
varOp = VarOp <$> notReserved
                 (overText wrapBackQuotes <$> betweenBackQuotes (ident lower)
                  <|> (operator opSymbol <|> simpleOperator))

ctorOp :: Parser CtorOp
ctorOp = CtorOp <$> notReserved
                    (overText wrapBackQuotes <$> betweenBackQuotes (ident upper)
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



ident :: Parser Char -> Parser Text
ident start = token $ pack <$> start <:> (idChar |*)

operator :: Parser Char -> Parser Text
operator start = token $ pack <$> start
                              <:> ((opSymbol <|> colon) |*)

simpleOperator :: Parser Text
simpleOperator = token $ oneOf [":"]

simpleOperatorFn :: Parser Text
simpleOperatorFn = token $ oneOf [",", ",,", ",,,"]

nonTokenQVar :: Parser QVar
nonTokenQVar = uncurry QVar <$> qTerm x
  where  x = Var <$> satisfy (`notElem` reservedKeyWords)
                    (nonTokenIdent lower)

nonTokenIdent :: Parser Char -> Parser Text
nonTokenIdent start = pack <$> start <:> (idChar |*)


idChar :: Parser Char
idChar = alphaNum <|> underscore <|> quote

opSymbol :: Parser Char
opSymbol = oneOf symbolChars

token :: Parser a -> Parser a
token = withTransform
  $ maybeSurroundedBy (anyComment |+) . maybeBetweenSpacing



qTerm :: Parser a -> Parser (Maybe Module, a)
qTerm x =  (,) <$> ((module'' <* dot) |?) <*> x


qTerm' :: (Text -> b) -> Parser (Maybe Module, b)
qTerm' fn = token parser
  where
    parser = do xs <- getComponents <$> module''
                pure (Module <$> wrapMaybe (initList xs)
                     , fn $ lastEx xs)
    getComponents (Module xs) = xs


anyComment :: Parser Text
anyComment = pragma <|> blockComment <|> lineComment

lineComment :: Parser Text
lineComment = string "--" ->>- (Text.singleton <$> newLine <|>
                          noneOf symbolChars ->>- (inverse newLine ||*))


blockComment :: Parser Text
blockComment = overText (wrap "{-"  "-}")
  <$> between (string "{-") (string "-}")
      (isNot "#" <> commentText)
  where
    commentText = fold <$> ((isNot "-" <> isNot "}") |*)


pragma :: Parser Text
pragma = overText (wrap "{-#"  "#-}")
   <$> between (string "{-#") (string "#-}")
  (isNot '#' ||*)

symbolChars :: [Char]
symbolChars =
     ['!', '#', '$', '%', '&', '*', '+', '.', '/',
      '<', '=', '>', '?', '@', '\\', '|', '^', '|',
      '-', '~']


notReserved :: Parser Text -> Parser Text
notReserved = satisfy
               (`notElem` (reservedSymbols ++ reservedKeyWords))


reservedKeyWords :: [Text]
reservedKeyWords = ["case","class","data","default","deriving",
                    "do","else","forall" ,"if","import","in",
                    "infix","infixl","infixr","instance",
                    "let","module" ,"newtype","of","qualified",
                    "then","type","where","_" ,"foreign",
                    "ccall","as","safe","unsafe"]

reservedSymbols :: [Text]
reservedSymbols = ["..","::","=","\\","|","<-","->","@","~","=>","[","]"]


betweenBackQuotes :: Parser b -> Parser b
betweenBackQuotes = surroundedBy (is '`')
