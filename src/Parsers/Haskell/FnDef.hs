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

import Bookhound.Parser              (Parser, andThen, satisfy, withError)
import Bookhound.ParserCombinators   (char, sepByOps, someSepBy, string, (->>-),
                                      (|*), (|+), (|?))
import Bookhound.Parsers.Char        (comma, dot)
import Bookhound.Parsers.Collections (listOf, tupleOf)
import Bookhound.Parsers.Number      (int)
import Bookhound.Parsers.Text        (anyString, betweenCurlyBrackets,
                                      betweenParens, betweenSquare, spacing)

import Control.Applicative ((<|>))
import Data.Foldable       (Foldable (fold))
import Data.Maybe          (maybeToList)
import Data.Text           (Text)
import Utils.String        (overText, wrapCurly)


fnSig :: Parser FnSig
fnSig = withError "Function signature" $
  FnSig <$> (var <* string "::")
        <*> type'


fnDef :: Parser FnDef
fnDef = withError "Function definition" $
  FnDef <$> (tupleOf var <|> pure <$> var)
        <*> (pattern' |*)
        <*> maybeGuardedFnBody (char '=')


infixAnnotation :: Parser InfixFnAnnotation
infixAnnotation = withError "Infix annotation" $
  InfixFnAnnotation <$> token (LAssoc <$ string "infixl" <|>
                               RAssoc <$ string "infixr")
                    <*> token int
                    <*> varOp


fnDefOrSig :: Parser FnDefOrSig
fnDefOrSig =  Def <$> fnDef <|>
              Sig <$> fnSig

fnBody :: Parser FnBody
fnBody = topLevelFnApply <|> openForm

  where
    topLevelFnApply = FnApply <$> delimitedForm
                              <*> (pure <$> lambdaExpr)

    fnApply = FnApply <$> delimitedForm
                      <*> (delimitedForm |+)

    infixFnApply = uncurry InfixFnApply <$>
      sepByOps fnOp (infixArgForm <|> betweenParens typeAnnotation)

    leftOpSection = uncurry LeftOpSection
      <$> betweenParens ((,) <$> fnOp <*> openForm)

    rightOpSection = uncurry RightOpSection
      <$> betweenParens ((,) <$> openForm <*> fnOp)

    opSection = leftOpSection <|> rightOpSection


    lambdaExpr = LambdaExpr <$> (char '\\' *> (pattern' |*))
                            <*> (string "->" *> openForm)

    letExpr = LetExpr <$> (string "let" *> betweenContext fnDefOrSig)
                      <*> (string "in"  *> openForm)

    whereExpr = WhereExpr <$> betweenCurlyBrackets openForm <* string "where"
                          <*> betweenContext fnDefOrSig

    ifExpr = IfExpr <$> (string "if"   *> openForm)
                    <*> (string "then" *> openForm)
                    <*> (string "else" *> openForm)

    multiWayIfExpr = MultiWayIfExpr <$>
      (string "if" *> betweenContext (guardedFnBody $ string "->"))

    doExpr = DoExpr <$> (string "do" *> betweenContext doStep)

    caseOfExpr = CaseOfExpr <$> (string "case" *> openForm <* string "of")
                            <*> betweenContext caseBinding

    lambdaCaseExpr = LambdaCaseExpr <$> (string "\\case" *> betweenContext caseBinding)

    listRange = betweenSquare $
      ListRange <$> (openForm <* string "..")
                <*> (openForm |?)

    typeAnnotation = TypeAnnotation <$> (infixArgForm <* string "::")
                                    <*> type'

    tuple = Tuple <$> tupleOf openForm

    list = List <$> listOf openForm

    fnOp = CtorOp' <$> qCtorOp
       <|> VarOp' <$> qVarOp

    fnOp' = FnOp' <$> betweenParens fnOp

    fnVar = FnVar' . Selector <$> betweenParens (dot *> var)
        <|> FnVar' <$>
            (Selection <$> nonTokenQVar <* dot <*> someSepBy dot var)
        <|> FnVar' . Var' <$> qVar
        <|> FnVar' . Ctor' <$> qCtor

    literal' = Literal' <$> literal

    recordCreate = RecordCreate <$> qCtor <*> recordFields

    recordUpdate = RecordUpdate <$> delimitedForm <*> recordFields

    recordFields = betweenCurlyBrackets (someSepBy comma recordField)

    recordField  = (,) <$> var <*> (char '=' *> openForm)

    infixArgForm = complexInfixForm <|> betweenParens complexInfixForm
               <|> singleForm

    openForm = complexForm <|> singleForm
               <|> betweenParens (complexForm <|> singleForm)

    delimitedForm = singleForm <|> betweenParens complexForm
                    <|> betweenParens singleForm

    singleForm = fnOp' <|> fnVar <|> literal' <|> tuple <|>
                 listRange <|> list <|> opSection

    complexForm = infixFnApply <|> complexInfixForm <|> typeAnnotation


    complexInfixForm = fnApply <|> lambdaCaseExpr <|>
                       lambdaExpr <|> letExpr <|> whereExpr <|>
                       ifExpr <|> multiWayIfExpr <|> doExpr <|>
                       caseOfExpr <|> betweenParens infixFnApply <|>
                       recordCreate <|> recordUpdate


doStep :: Parser DoStep
doStep = DoBinding  <$> (tupleOf var <|> pure <$> var) <* string "<-"
                    <*> (adaptFnBody `andThen` fnBody) <|>
         LetBinding <$> (string "let" *> betweenContext fnDefOrSig) <|>
         Body       <$> (adaptFnBody `andThen` fnBody)


caseBinding :: Parser CaseBinding
caseBinding = CaseBinding <$> pattern' <*> maybeGuardedFnBody (string "->")


maybeGuardedFnBody :: Parser a -> Parser MaybeGuardedFnBody
maybeGuardedFnBody sep = Guarded  <$> (guardedFnBody sep |+) <|>
                         Standard <$> (sep *> (adaptFnBody `andThen` fnBody))

guardedFnBody :: Parser a -> Parser GuardedFnBody
guardedFnBody sep = GuardedFnBody <$> guard <* sep <*> (adaptFnBody `andThen` fnBody)

guard :: Parser Guard
guard = char '|' *>
    (Otherwise <$ token (string "otherwise") <|>
     Guard     <$> someSepBy comma patternGuard)


patternGuard :: Parser PatternGuard
patternGuard = PatternGuard <$> (pattern' <* string "<-") <*> fnBody <|>
               SimpleGuard  <$> fnBody


adaptFnBody :: Parser Text
adaptFnBody = do start <- otherText
                 next <- ((string "where" <> anyString) |?)
                 other <- ((char ';' ->>- anyString) |?)
                 let x = maybe start (overText wrapCurly start <>) next <> fold other
                 pure x
 where
   otherText = (fold <$> (spacing |?)) <> (fold <$> (textElem |*))
   textElem = satisfy (`notElem` ["where", ";"]) lexeme ->>- (spacing |?)


statements :: Parser a -> Parser [a]
statements parser = fold <$> someSepBy (char ';') (maybeToList <$> (parser |?))

betweenContext :: Parser a -> Parser [a]
betweenContext = betweenCurlyBrackets . statements

betweenContextTupled :: Parser a1 -> Parser a2 -> Parser ([a1], [a2])
betweenContextTupled p1 p2 = betweenCurlyBrackets $
                             (,) <$> statements p1 <*> statements p2
