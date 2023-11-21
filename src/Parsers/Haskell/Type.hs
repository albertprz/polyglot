module Parsers.Haskell.Type where

import ClassyPrelude

import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (multipleSepBy, string, (|+))
import Bookhound.Parsers.Char        (comma, dot, lower, upper)
import Bookhound.Parsers.Collections (tupleOf)
import Bookhound.Parsers.Text        (betweenParens, betweenSquare,
                                      maybeBetweenParens)

import Parsers.Haskell.Common   (ident, notReserved, qClass, qTerm')
import SyntaxTrees.Haskell.Type (AnyKindedType (..), ClassConstraint (..),
                                 QTypeCtor (QTypeCtor), QTypeVar (QTypeVar),
                                 Type (..), TypeCtor (..), TypeParam (..),
                                 TypeVar (..))

typeParam :: Parser TypeParam
typeParam = TypeParam <$> notReserved (ident lower)

typeVar :: Parser TypeVar
typeVar = TypeVar  <$> ident upper <|>
          UnitType <$ string "()"

typeCtor :: Parser TypeCtor
typeCtor = TypeCtor  <$> ident upper   <|>
           Arrow     <$  string "(->)" <|>
           TupleType <$  string "(,)"  <|>
           ListType  <$  string "[]"

anyKindedType :: Parser AnyKindedType
anyKindedType = TypeValue <$> type'    <|>
                TypeFn    <$> qTypeCtor


classConstraints :: Parser Type -> Parser [ClassConstraint]
classConstraints typeParser =
  tupleOf (classConstraint typeParser)
  <|> pure <$> maybeBetweenParens (classConstraint typeParser)


classConstraint :: Parser Type -> Parser ClassConstraint
classConstraint typeParser = ClassConstraint <$> qClass <*> (typeParser |+)


standaloneType :: Parser Type
standaloneType = typeParam' <|> typeVar' <|> tuple <|> list <|> betweenParens type'
  where
    typeParam' = TypeParam' <$> typeParam
    typeVar' = TypeVar' <$> (qTypeVar <|> QTypeVar Nothing <$> typeVar)
    tuple = CtorTypeApply (QTypeCtor Nothing TupleType)
            <$> betweenParens (multipleSepBy comma type')
    list  = CtorTypeApply (QTypeCtor Nothing ListType)
            <$> (pure <$> betweenSquare type')


type' :: Parser Type
type' = typeScope <|> classScope <|> type'' <|> maybeBetweenParens type''
  where
    type'' = arrow <|> typeApply <|> elem'

    typeApply = CtorTypeApply   <$> typeCtor' <*> (typeApplyElem |+) <|>
                ParamTypeApply  <$> typeParam <*> (typeApplyElem |+) <|>
                NestedTypeApply <$> betweenParens typeApply <*> (typeApplyElem |+)

    arrow = CtorTypeApply (QTypeCtor Nothing Arrow)
            <$> multipleSepBy (string "->") arrowElem
    tuple = CtorTypeApply (QTypeCtor Nothing TupleType)
            <$> betweenParens (multipleSepBy comma type'')
    list  = CtorTypeApply (QTypeCtor Nothing ListType)
            <$> (pure <$> betweenSquare type'')

    typeCtor'  = qTypeCtor <|> QTypeCtor Nothing <$> typeCtor
    typeVar'   = TypeVar'   <$> (qTypeVar <|> QTypeVar Nothing <$> typeVar)
    typeParam' = TypeParam' <$> typeParam

    typeScope = TypeScope <$> (string "forall" *> (typeParam |+) <* dot)
                          <*> (classScope <|> type'')
    classScope = ClassScope <$> (classConstraints' <* string "=>")
                            <*> type''

    classConstraints' = classConstraints
                        (elem' <|> betweenParens (arrow <|> typeApply))


    typeApplyElem = elem' <|> betweenParens (arrow <|> typeApply)
    arrowElem     = typeApply <|> elem' <|> betweenParens arrow

    elem' = typeVar' <|> typeParam' <|> tuple <|> list <|>
            betweenParens (typeScope <|> classScope)

qTypeVar :: Parser QTypeVar
qTypeVar = uncurry QTypeVar <$> qTerm' TypeVar

qTypeCtor :: Parser QTypeCtor
qTypeCtor = uncurry QTypeCtor <$> qTerm' TypeCtor
