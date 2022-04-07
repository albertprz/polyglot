module Parsers.Haskell.Type where

import SyntaxTrees.Haskell.Common ()
import SyntaxTrees.Haskell.Type
    ( Type(..), ClassConstraint(..), AnyKindedType(..),
      TypeCtor(..), TypeVar(..), TypeParam(..) )

import Parser ( Parser )
import ParserCombinators
    ( someSeparatedBy, manySeparatedBy, (|+), (<|>), IsMatch(is) )
import Parsers.String
    ( maybeWithinParens, withinSquareBrackets, withinParens )
import Parsers.Char ( comma, dot, upper, lower )

import Parsers.Number ()
import Parsers.Collections ()
import Parsers.Haskell.Common ( class', ident )



typeParam :: Parser TypeParam
typeParam = TypeParam <$> ident lower

typeVar :: Parser TypeVar
typeVar = TypeVar  <$> ident upper <|>
          UnitType <$  is "()"

typeCtor :: Parser TypeCtor
typeCtor = TypeCtor  <$> ident upper <|>
           Arrow     <$  is "(->)"   <|>
           TupleType <$  is "(,)"    <|>
           ListType  <$  is "[]"

anyKindedType :: Parser AnyKindedType
anyKindedType = TypeValue <$> type'   <|>
                TypeFn    <$> typeCtor

classConstraint :: Parser ClassConstraint
classConstraint = ClassConstraint <$> class' <*> type'


type' :: Parser Type
type' = maybeWithinParens $ typeScope <|> classScope <|> type''
  where
  type''     = arrow <|> typeApply  <|> elem'

  typeApply  = CtorTypeApply   <$> typeCtor               <*> (typeApplyElem |+) <|>
               ParamTypeApply  <$> typeParam              <*> (typeApplyElem |+) <|>
               NestedTypeApply <$> withinParens typeApply <*> (typeApplyElem |+)

  arrow      = CtorTypeApply Arrow     <$> manySeparatedBy (is "->") arrowElem
  tuple      = CtorTypeApply TupleType <$> (withinParens $ manySeparatedBy comma type'')
  list       = CtorTypeApply ListType  <$> ((: []) <$> withinSquareBrackets type'')
  typeVar'   = TypeVar'                <$> typeVar
  typeParam' = TypeParam'              <$> typeParam

  typeScope  = TypeScope  <$> (is "forall" *> someSeparatedBy dot typeParam <* dot)
                          <*> (classScope <|> type'')
  classScope = ClassScope <$> (((withinParens $ manySeparatedBy comma classConstraint') <|>
                                (: []) <$> classConstraint) <* (is "=>"))
                          <*> type''

  classConstraint' = ClassConstraint <$> class'
                                     <*> (elem' <|> withinParens (arrow <|> typeApply))

  typeApplyElem = elem'     <|> withinParens (arrow <|> typeApply)
  arrowElem     = typeApply <|> elem'      <|> withinParens arrow
  elem'         = typeVar'  <|> typeParam' <|> tuple <|> list  <|>
                  withinParens (typeScope  <|> classScope)
