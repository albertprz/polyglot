module Parsers.Haskell.Type where


import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (IsMatch (is), multipleSepBy, (<|>),
                                      (|+))
import Bookhound.Parsers.Char        (comma, dot, lower, upper)
import Bookhound.Parsers.Collections (tupleOf)
import Bookhound.Parsers.String      (maybeWithinParens, withinParens,
                                      withinSquareBrackets)

import Parsers.Haskell.Common   (ident, notReserved, qClass, qTerm')
import SyntaxTrees.Haskell.Type (AnyKindedType (..), ClassConstraint (..),
                                 QTypeCtor (QTypeCtor), QTypeVar (QTypeVar),
                                 Type (..), TypeCtor (..), TypeParam (..),
                                 TypeVar (..))

typeParam :: Parser TypeParam
typeParam = TypeParam <$> notReserved (ident lower)

typeVar :: Parser TypeVar
typeVar = TypeVar  <$> ident upper <|>
          UnitType <$ is "()"

typeCtor :: Parser TypeCtor
typeCtor = TypeCtor  <$> ident upper <|>
           Arrow     <$  is "(->)"   <|>
           TupleType <$  is "(,)"    <|>
           ListType  <$  is "[]"

anyKindedType :: Parser AnyKindedType
anyKindedType = TypeValue <$> type'     <|>
                TypeFn    <$> qTypeCtor


classConstraints :: Parser Type -> Parser [ClassConstraint]
classConstraints typeParser =
  tupleOf (classConstraint typeParser)
  <|> pure <$> maybeWithinParens (classConstraint typeParser)


classConstraint :: Parser Type -> Parser ClassConstraint
classConstraint typeParser = ClassConstraint <$> qClass <*> (typeParser |+)



type' :: Parser Type
type' = typeScope <|> classScope <|> type'' <|> maybeWithinParens (type'')
  where
    type'' = arrow <|> typeApply <|> elem'

    typeApply = CtorTypeApply   <$> typeCtor' <*> (typeApplyElem |+) <|>
                ParamTypeApply  <$> typeParam <*> (typeApplyElem |+) <|>
                NestedTypeApply <$> withinParens typeApply <*> (typeApplyElem |+)

    arrow = CtorTypeApply (QTypeCtor Nothing Arrow)
            <$> multipleSepBy (is "->") arrowElem
    tuple = CtorTypeApply (QTypeCtor Nothing TupleType)
            <$> (withinParens $ multipleSepBy comma type'')
    list  = CtorTypeApply (QTypeCtor Nothing ListType)
            <$> (pure <$> withinSquareBrackets type'')

    typeCtor'  = qTypeCtor <|> QTypeCtor Nothing <$> typeCtor
    typeVar'   = TypeVar'   <$> (qTypeVar <|> QTypeVar Nothing <$> typeVar)
    typeParam' = TypeParam' <$> typeParam

    typeScope = TypeScope <$> (is "forall" *> (typeParam |+) <* dot)
                          <*> (classScope <|> type'')
    classScope = ClassScope <$> (classConstraints' <* (is "=>"))
                            <*> type''

    classConstraints' = classConstraints
                        (elem' <|> withinParens (arrow <|> typeApply))


    typeApplyElem = elem' <|> withinParens (arrow <|> typeApply)
    arrowElem     = typeApply <|> elem' <|> withinParens arrow

    elem' = typeVar' <|> typeParam' <|> tuple <|> list <|>
            withinParens (typeScope <|> classScope)

qTypeVar :: Parser QTypeVar
qTypeVar = uncurry QTypeVar <$> qTerm' TypeVar

qTypeCtor :: Parser QTypeCtor
qTypeCtor = uncurry QTypeCtor <$> qTerm' TypeCtor
