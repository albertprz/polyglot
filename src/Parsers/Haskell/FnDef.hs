module Parsers.Haskell.FnDef where

import Lexers.Haskell.Layout     (lexeme)
import Parsers.Haskell.Common    (literal, nonTokenQVar, qCtor, qCtorOp, qVar,
                                  qVarOp, token, var, varOp)
import Parsers.Haskell.Pattern   (pattern')
import Parsers.Haskell.Type      (type')
import SyntaxTrees.Haskell.FnDef (Associativity (LAssoc, RAssoc),
                                  CaseBinding (..), DoStep (..), FnBody (..),
                                  FnDef (FnDef), FnDefOrSig (..), FnOp (..),
                                  FnSig (..), FnVar (..), Guard (..),
                                  GuardedFnBody (..),
                                  InfixFnAnnotation (InfixFnAnnotation),
                                  MaybeGuardedFnBody (..), PatternGuard (..))

import Bookhound.Parser              (Parser, andThen, check)
import Bookhound.ParserCombinators   (IsMatch (is), someSepBy, (<|>), (->>-),
                                      (|*), (|+), (|?))
import Bookhound.Parsers.Char        (comma, dot)
import Bookhound.Parsers.Collections (listOf, tupleOf)
import Bookhound.Parsers.Number      (int)
import Bookhound.Parsers.String      (spacing, string, withinCurlyBrackets,
                                      withinParens)

import Data.Foldable (Foldable (fold))
import Data.Maybe    (maybeToList)
import Utils.String  (wrapCurly)


fnSig :: Parser FnSig
fnSig = FnSig <$> (var <* is "::")
              <*> type'

fnDef :: Parser FnDef
fnDef = FnDef <$> (tupleOf var <|> pure <$> var)
              <*> (pattern' |*)
              <*> maybeGuardedFnBody (is "=")


infixAnnotation :: Parser InfixFnAnnotation
infixAnnotation = InfixFnAnnotation
  <$> token (LAssoc <$ is "infixl" <|> RAssoc <$ is "infixr")
  <*> token int
  <*> varOp


fnDefOrSig :: Parser FnDefOrSig
fnDefOrSig = Def <$> fnDef <|>
             Sig <$> fnSig

fnBody :: Parser FnBody
fnBody = openForm

  where
    fnApply = FnApply <$> delimitedForm <*> (delimitedForm |+)

    infixFnApply = uncurry InfixFnApply <$> sepByOps fnOp
       (complexInfixForm <|> withinParens complexInfixForm <|> singleForm)

    leftOpSection = uncurry LeftOpSection
      <$> withinParens ((,) <$> fnOp <*> openForm)

    rightOpSection = uncurry RightOpSection
      <$> withinParens ((,) <$> openForm <*> fnOp)

    postfixOpSection = uncurry PostFixOpSection
      <$> withinParens ((,) <$> openForm <*> fnOp)


    opSection = leftOpSection <|> rightOpSection


    lambdaExpr = LambdaExpr <$> (is '\\' *> (tupleOf var <|> (var |*)))
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

    lambdaCaseExpr = LambdaCaseExpr <$> (is "\\case" *> withinContext caseBinding)

    tuple = Tuple <$> tupleOf openForm

    list = List <$> listOf openForm

    fnOp = CtorOp' <$> qCtorOp <|> VarOp' <$> qVarOp

    fnVar = FnVar' . Selector <$> withinParens (dot *> var)              <|>
            FnVar' <$> (Selection <$> nonTokenQVar <* dot <*> someSepBy dot var) <|>
            FnVar' . Var' <$> qVar                                       <|>
            FnVar' . Ctor' <$> qCtor

    literal' = Literal' <$> literal

    recordCreate = RecordCreate <$> qCtor <*> recordFields

    recordUpdate = RecordUpdate <$> qVar <*> recordFields

    recordFields = withinCurlyBrackets (someSepBy comma recordField)

    recordField   = (,) <$> var <*> (is "=" *> openForm)

    openForm = complexForm <|> singleForm
               <|> withinParens (complexForm <|> singleForm)

    delimitedForm = singleForm <|> withinParens complexForm
                    <|> withinParens singleForm

    singleForm = fnVar <|> literal' <|> tuple <|> list <|> opSection
                       <|> postfixOpSection

    complexForm = infixFnApply <|> complexInfixForm
                  <|> recordCreate <|> recordUpdate

    complexInfixForm = fnApply <|> lambdaCaseExpr <|>
                       lambdaExpr <|> letExpr <|> whereExpr <|>
                       ifExpr <|> multiWayIfExpr <|> doExpr <|>
                       caseOfExpr <|> withinParens infixFnApply


doStep :: Parser DoStep
doStep = DoBinding  <$> (tupleOf var <|> pure <$> var) <* is "<-"
                    <*> (adaptFnBody `andThen` fnBody) <|>
         LetBinding <$> (is "let" *> withinContext fnDefOrSig) <|>
         Body       <$> (adaptFnBody `andThen` fnBody)

caseBinding :: Parser CaseBinding
caseBinding = CaseBinding <$> pattern' <*> maybeGuardedFnBody (is "->")

maybeGuardedFnBody :: Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep = Guarded  <$> (guardedFnBody sep |+) <|>
                         Standard <$> (sep *> (adaptFnBody `andThen` fnBody))

guardedFnBody :: Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*> (adaptFnBody `andThen` fnBody)

guard :: Parser Guard
guard = Otherwise <$  (is "|" *> token (is "otherwise")) <|>
        Guard     <$> (is "|" *> someSepBy comma patternGuard)


patternGuard :: Parser PatternGuard
patternGuard = PatternGuard <$> (pattern' <* is "<-") <*> fnBody <|>
               SimpleGuard  <$> fnBody


adaptFnBody :: Parser String
adaptFnBody = do start <- otherText
                 next <- ((is "where" ->>- string) |?)
                 other <- ((is ";" ->>- string) |?)
                 let x = maybe start ((wrapCurly start) ++) next ++ fold other
                 pure x


otherText :: Parser String
otherText = (spacing |?) ->>- (textElem |*)

 where
   textElem = check "" (`notElem` ["where", ";"]) lexeme ->>- (spacing |?)


statements :: Parser a -> Parser [a]
statements parser = fold <$> someSepBy (is ";") (maybeToList <$> (parser |?))

withinContext :: Parser a -> Parser [a]
withinContext = withinCurlyBrackets . statements

withinContextTupled :: Parser a1 -> Parser a2 -> Parser ([a1], [a2])
withinContextTupled p1 p2 = withinCurlyBrackets $
                               (,) <$> statements p1 <*> statements p2


sepByOps :: Parser a -> Parser b -> Parser ([a], [b])
sepByOps sep p = do x <-  p
                    y <- (((,) <$> sep <*> p) |+)
                    pure $ (fst <$> y, x : (snd <$> y))
