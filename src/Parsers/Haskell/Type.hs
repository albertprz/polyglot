module Parsers.Haskell.Type where

import Parser                     (Parser)
import ParserCombinators          (IsMatch (is), manySepBy, someSepBy, (<|>),
                                   (|+))
import Parsers.Char               (comma, dot, lower, upper)
import Parsers.Collections        (tupleOf)
import Parsers.Haskell.Common     (class', ident)
import Parsers.Number             ()
import Parsers.String             (maybeWithinParens, withinParens,
                                   withinSquareBrackets)
import SyntaxTrees.Haskell.Common ()
import SyntaxTrees.Haskell.Type   (AnyKindedType (..), ClassConstraint (..),
                                   Type (..), TypeCtor (..), TypeParam (..),
                                   TypeVar (..))

typeParam :: Parser TypeParam
typeParam = TypeParam <$> ident lower

typeVar :: Parser TypeVar
typeVar = TypeVar  <$> ident upper <|>
          UnitType <$ is "()"

typeCtor :: Parser TypeCtor
typeCtor = TypeCtor  <$> ident upper <|>
           Arrow     <$  is "(->)" <|>
           TupleType <$  is "(,)" <|>
           ListType  <$  is "[]"

anyKindedType :: Parser AnyKindedType
anyKindedType = TypeValue <$> type' <|>
                TypeFn    <$> typeCtor


classConstraints :: Parser Type -> Parser [ClassConstraint]
classConstraints typeParser = tupleOf (classConstraint typeParser) <|>
                (: []) <$> classConstraint typeParser

classConstraint :: Parser Type -> Parser ClassConstraint
classConstraint typeParser = ClassConstraint <$> class' <*> typeParser


type' :: Parser Type
type' = maybeWithinParens $ typeScope <|> classScope <|> type''
  where
    type'' = arrow <|> typeApply <|> elem'

    typeApply = CtorTypeApply   <$> typeCtor <*> (typeApplyElem |+) <|>
                ParamTypeApply  <$> typeParam <*> (typeApplyElem |+) <|>
                NestedTypeApply <$> withinParens typeApply <*> (typeApplyElem |+)

    arrow = CtorTypeApply Arrow     <$> manySepBy (is "->") arrowElem
    tuple = CtorTypeApply TupleType <$> (withinParens $ manySepBy comma type'')
    list  = CtorTypeApply ListType  <$> ((: []) <$> withinSquareBrackets type'')

    typeVar'   = TypeVar'   <$> typeVar
    typeParam' = TypeParam' <$> typeParam

    typeScope = TypeScope <$> (is "forall" *> someSepBy dot typeParam <* dot)
                          <*> (classScope <|> type'')
    classScope = ClassScope <$> (classConstraints' <* (is "=>"))
                            <*> type''

    classConstraints' = classConstraints
                        (elem' <|> withinParens (arrow <|> typeApply))


    typeApplyElem = elem' <|> withinParens (arrow <|> typeApply)
    arrowElem     = typeApply <|> elem' <|> withinParens arrow

    elem' = typeVar' <|> typeParam' <|> tuple <|> list <|>
            withinParens (typeScope <|> classScope)
