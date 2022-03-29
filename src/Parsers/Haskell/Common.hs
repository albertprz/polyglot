module Parsers.Haskell.Common  where

import SyntaxTrees.Haskell.Common ( Type(..), Module(..), Class(..), TypeCtor(..),
                                    TypeParam(..), CtorOp(..), VarOp(..),
                                    Literal(..), Var(..), Ctor(..), TypeVar(..),
                                    AnyKindedType (..))

import Parser (Parser, runParser, withTransform, exactly)
import ParserCombinators ( (<|>), (|*), (|+), IsMatch (oneOf, is, isNot),
                           maybeWithin, manySeparatedBy, anySeparatedBy)
import Parsers.String ( withinDoubleQuotes, withinQuotes, spacing, withinParens,
                        withinSquareBrackets)
import Parsers.Char

import Parsers.Number ( double, int )


literal :: Parser Literal
literal = IntLit . show    <$> int               <|>
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

typeParam :: Parser TypeParam
typeParam = TypeParam <$> ident lower

typeVar :: Parser TypeVar
typeVar = TypeVar <$> ident upper <|>
          Unit    <$  is "()"

typeCtor :: Parser TypeCtor
typeCtor = TypeCtor <$> ident upper <|>
           Arrow    <$  is "(->)"   <|>
           Tuple    <$  is "(,)"    <|>
           List     <$  is "[]"

anyKindedType :: Parser AnyKindedType
anyKindedType = TypeValue <$> type'   <|>
                TypeFn    <$> typeCtor

class' :: Parser Class
class' = Class <$> ident upper

module' :: Parser Module
module' = Module <$> ((:) <$> ident upper <*> ((dot *> ident upper) |*))

type' :: Parser Type
type' = arrow <|> typeApply <|> elem'
  where
    typeApply  = CtorTypeApply   <$> typeCtor               <*> (typeApplyElem |+) <|>
                 ParamTypeApply  <$> typeParam              <*> (typeApplyElem |+) <|>
                 NestedTypeApply <$> withinParens typeApply <*> (typeApplyElem |+)

    arrow      = CtorTypeApply Arrow <$> manySeparatedBy (is "->") arrowElem
    tuple      = CtorTypeApply Tuple <$> (withinParens $ manySeparatedBy comma type')
    list       = CtorTypeApply List  <$> ((: []) <$> withinSquareBrackets type')
    typeVar'   = TypeVar'   <$> typeVar
    typeParam' = TypeParam' <$> typeParam

    typeApplyElem = elem'     <|> withinParens (arrow <|> typeApply)
    arrowElem     = typeApply <|> elem'      <|> withinParens arrow



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
