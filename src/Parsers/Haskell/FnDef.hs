module Parsers.Haskell.FnDef where

import Data.Foldable             (Foldable (fold))
import Data.Maybe                (maybeToList)
import Parser                    (Parser)
import ParserCombinators         (IsMatch (is), sepByOp, someSepBy, (<|>), (|*),
                                  (|+), (|?))
import Parsers.Char              (comma)
import Parsers.Collections       (listOf, tupleOf)
import Parsers.Haskell.Common    (ctor, ctorOp, literal, var, varOp)
import Parsers.Haskell.Pattern   (pattern')
import Parsers.Haskell.Type      (type')
import Parsers.String            (withinCurlyBrackets, withinParens)
import SyntaxTrees.Haskell.FnDef (CaseBinding (..), DoStep (..), FnBody (..),
                                  FnDef (FnDef), FnDefOrSig (..), FnOp (..),
                                  FnSig (..), FnVar (..), Guard (..),
                                  GuardedFnBody (..), MaybeGuardedFnBody (..),
                                  PatternGuard (..))


fnSig :: Parser FnSig
fnSig = FnSig <$> (var <* is "::") <*> type'

fnDef :: Parser FnDef
fnDef = FnDef <$> var <*> (pattern' |*)
                      <*> maybeGuardedFnBody (is "=")

fnDefOrSig :: Parser FnDefOrSig
fnDefOrSig = Def <$> fnDef <|>
             Sig <$> fnSig

fnBody :: Parser FnBody
fnBody = openForm
  where
    fnApply = FnApply <$> delimitedForm <*> (delimitedForm |+)

    infixFnApply = uncurry InfixFnApply <$> sepByOp fnOp (complexInfixForm <|>
                                                          singleForm)

    lambdaExpr = LambdaExpr <$> (is '\\' *> someSepBy comma var)
                            <*> (is "->" *> openForm)

    letExpr = LetExpr <$> (is "let" *> withinContext fnDefOrSig)
                      <*> (is "in"  *> fnBody)

    whereExpr = WhereExpr <$> withinCurlyBrackets fnBody <* is "where"
                          <*> withinContext fnDefOrSig

    ifExpr = IfExpr <$> (is "if"   *> openForm)
                    <*> (is "then" *> openForm)
                    <*> (is "else" *> openForm)

    multiWayIfExpr = MultiWayIfExpr <$> withinContext (guardedFnBody (is "->"))

    doExpr = DoExpr <$> (is "do" *> withinContext doStep)

    caseOfExpr = CaseOfExpr <$> (is "case" *> openForm <* is "of")
                            <*> withinContext caseBinding

    tuple = Tuple <$> tupleOf openForm

    list = List <$> listOf openForm

    fnOp = VarOp' <$> varOp <|>
           CtorOp' <$> ctorOp

    fnVar = FnVar' . Var' <$> var <|>
            FnVar' . Ctor' <$> ctor

    literal' = Literal' <$> literal

    openForm = complexForm <|> singleForm

    delimitedForm = singleForm <|> withinParens complexForm

    singleForm = fnVar <|> literal' <|> tuple <|> list

    complexForm = infixFnApply <|> complexInfixForm

    complexInfixForm = fnApply <|> lambdaExpr <|> letExpr <|> whereExpr <|>
                       ifExpr <|> multiWayIfExpr <|> doExpr <|>
                       caseOfExpr <|> withinParens infixFnApply


doStep :: Parser DoStep
doStep = DoBinding <$> var <* is "<-" <*> fnBody <|>
         Body      <$> fnBody

caseBinding :: Parser CaseBinding
caseBinding = CaseBinding <$> pattern' <*> maybeGuardedFnBody (is "->")

maybeGuardedFnBody :: Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep = Guarded  <$> withinContext (guardedFnBody sep) <|>
                         Standard <$> (sep *> fnBody)

guardedFnBody :: Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*> fnBody

guard :: Parser Guard
guard = Guard     <$> (is "|" *> someSepBy comma patternGuard) <|>
        Otherwise <$ is "otherwise"

patternGuard :: Parser PatternGuard
patternGuard = PatternGuard <$> (pattern' <* is "<-") <*> fnBody <|>
               SimpleGuard  <$> fnBody

withinContext :: Parser a -> Parser [a]
withinContext parser = fold <$> (withinCurlyBrackets $ someSepBy (is ";")
                        (maybeToList <$> (parser |?)))
