module Parsers.Haskell.Common  where

import SyntaxTrees.Haskell.Common ( Type(..), Module(..), Class(..), TypeCtor(..),
                                    TypeParam(..), CtorOp(..), VarOp(..),
                                    Literal(..), Var(..), Ctor(..), TypeVar(..) )

import Parser (Parser)
import ParserCombinators ( (<#>), (<|>), (|*), (|+), (>>>), IsMatch (oneOf, is, isNot),
                           maybeWithin )
import Parsers.String ( withinDoubleQuotes, withinQuotes, spacing, withinParens)
import Parsers.Char (lower, alphaNum, underscore, quote, upper, char, colon, dot)
import Parsers.Number ( double, int )


literal :: Parser Literal
literal = IntLit . show <$> int <|>
          FloatLit . show <$> double <|>
          CharLit <$> withinQuotes char <#> 1 <|>
          StringLit <$> withinDoubleQuotes (isNot '"' |*)

var :: Parser Var
var = Var <$> ident lower

ctor :: Parser Ctor
ctor = Ctor <$> ident upper

varOp :: Parser VarOp
varOp = VarOp <$> operator opChar

ctorOp :: Parser CtorOp
ctorOp = CtorOp <$> operator colon


typeVar :: Parser TypeVar
typeVar = TypeVar <$> ident upper

typeCtor :: Parser TypeCtor
typeCtor = TypeCtor <$> ident upper

typeParam :: Parser TypeParam
typeParam = TypeParam <$> ident lower

class' :: Parser Class
class' = Class <$> ident upper

module' :: Parser Module
module' = Module <$> (ident upper >>> ((dot >>> ident upper) |*))

type' :: Parser Type
type' = maybeWithin spacing $
          arrow <|> typeApply <|> typeApply <|> arrow   where

  typeVar'   = TypeVar' <$> typeVar
  typeParam' = TypeParam' <$> typeParam
  typeApply  = TypeApply <$> typeCtor <*> (typeApplyElem |+)
  arrow      = Arrow <$> ((:) <$> arrowElem <*> ((is "->" *> arrowElem) |+))

  typeApplyElem = maybeWithin spacing $ withinParens (arrow <|> typeApply) <|>
    typeVar' <|> typeParam'
  arrowElem =  maybeWithin spacing $ withinParens arrow <|> typeApply <|>
    typeVar' <|> typeParam'



ident :: Parser Char -> Parser String
ident start = (:) <$> start <*> (idChar |*)

operator :: Parser Char -> Parser String
operator start = (:) <$> start <*> ((opChar <|> colon) |*)

idChar :: Parser Char
idChar = alphaNum <|> underscore <|> quote

opChar :: Parser Char
opChar = oneOf ['!', '#', '$', '%', '&', '⋆', '+', '.', '/', '<', '=',
                '>', '?', '@', '\\', '|', '^', '|', '-', '~']
