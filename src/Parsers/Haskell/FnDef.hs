module Parsers.Haskell.FnDef where

import Data.Foldable             (Foldable (fold))
import Data.Maybe                (maybeToList)
import Lexers.Haskell.Layout     (lexeme)
import Parser
import ParserCombinators
import Parsers.Char              (comma, dot)
import Parsers.Collections       (listOf, tupleOf)
import Parsers.Haskell.Common    (literal, qCtor, qCtorOp, qVar, qVarOp, var)
import Parsers.Haskell.Pattern   (pattern')
import Parsers.Haskell.Type      (type')
import Parsers.String            (spacing, string, withinCurlyBrackets,
                                  withinParens)
import SyntaxTrees.Haskell.FnDef (CaseBinding (..), DoStep (..), FnBody (..),
                                  FnDef (FnDef), FnDefOrSig (..), FnOp (..),
                                  FnSig (..), FnVar (..), Guard (..),
                                  GuardedFnBody (..), MaybeGuardedFnBody (..),
                                  PatternGuard (..))
import Utils.String              (wrapCurly)

-- TODO: Review fn guards: (| x == 1)
-- TODO: Support Tuple definitions: (def1, def2)
-- TODO: Support Lambda Case exprs : (\case)
-- TODO: Support parsing operators with different precedence

fnSig :: Parser FnSig
fnSig = FnSig <$> (var <* is "::") <*> type'

fnDef :: Parser FnDef
fnDef = FnDef <$> var <*> (pattern' |*)
                      <*> maybeGuardedFnBody (is "=")

fnDefOrSig :: Parser FnDefOrSig
fnDefOrSig = Def <$> fnDef <|>
             Sig <$> fnSig

fnBody :: Parser FnBody
fnBody = adaptFnBody `andThen` openForm

  where
    fnApply = FnApply <$> delimitedForm <*> (delimitedForm |+)

    infixFnApply = uncurry InfixFnApply <$> sepByOp fnOp (complexInfixForm <|>
                                                withinParens complexInfixForm <|>
                                                          singleForm)

    leftOpSection = uncurry LeftOpSection <$>
      withinParens ((,) <$> fnOp <*> delimitedForm)

    rightOpSection = uncurry RightOpSection <$>
      withinParens ((,) <$> delimitedForm <*> fnOp)

    postfixOpSection = uncurry PostFixOpSection <$>
      withinParens ((,) <$> openForm <*> fnOp)


    opSection = leftOpSection <|> rightOpSection


    lambdaExpr = LambdaExpr <$> (is '\\' *> someSepBy comma var)
                            <*> (is "->" *> openForm)

    letExpr = LetExpr <$> (is "let" *> withinContext fnDefOrSig)
                      <*> (is "in"  *> openForm)

    whereExpr = WhereExpr <$> withinCurlyBrackets openForm <* is "where"
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

    fnOp = VarOp' <$> qVarOp <|>
           CtorOp' <$> qCtorOp

    fnVar = FnVar' . Selector <$> withinParens (is "." *> var)          <|>
            FnVar' <$> (Selection <$> qVar <* dot <*> anySepBy dot var) <|>
            FnVar' . Var' <$> qVar                                      <|>
            FnVar' . Ctor' <$> qCtor

    literal' = Literal' <$> literal

    openForm = complexForm <|> singleForm

    delimitedForm = singleForm <|> withinParens complexForm

    singleForm = fnVar <|> literal' <|> tuple <|> list <|> opSection
                       <|> postfixOpSection

    complexForm = infixFnApply <|> complexInfixForm

    complexInfixForm = fnApply <|> lambdaExpr <|> letExpr <|> whereExpr <|>
                       ifExpr <|> multiWayIfExpr <|> doExpr <|>
                       caseOfExpr <|> withinParens infixFnApply


doStep :: Parser DoStep
doStep = DoBinding <$> var <* is "<-" <*> fnBody <|>
         LetBinding <$> (is "let" *> withinContext fnDefOrSig) <|>
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


adaptFnBody :: Parser String
adaptFnBody = do start <- otherText
                 next <- ((is "where" >>> string) |?)
                 other <- ((is ";" >>> string) |?)
                 let x = (maybe start ((wrapCurly start) ++) next) ++ fold other
                 pure x



otherText :: Parser String
otherText = mconcat <$>
           (pure <$> (spacing |?) >>>
            (((check "" (`notElem`  ["where", ";"]) lexeme) >>> (spacing |?)) |*))


statements :: Parser a -> Parser [a]
statements parser = fold <$> someSepBy (is ";") (maybeToList <$> (parser |?))

withinContext :: Parser a -> Parser [a]
withinContext = withinCurlyBrackets . statements

withinContextTupled :: Parser a1 -> Parser a2 -> Parser ([a1], [a2])
withinContextTupled p1 p2 = withinCurlyBrackets $
                               (,) <$> statements p1 <*> statements p2
