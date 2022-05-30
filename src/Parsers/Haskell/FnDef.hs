module Parsers.Haskell.FnDef  where

import SyntaxTrees.Haskell.Common
import SyntaxTrees.Haskell.Type
import SyntaxTrees.Haskell.Pattern
import SyntaxTrees.Haskell.FnDef
import Parsers.Haskell.Common
import Parsers.Haskell.Type
import Parsers.Haskell.Pattern

import Parser
import ParserCombinators
import Parsers.String
import Parsers.Char
import Parsers.Collections (tupleOf, listOf)



fnSig :: Parser FnSig
fnSig = FnSig <$> (var <* is "::") <*> type'

fnDef :: Parser FnDef
fnDef = FnDef <$> var <*> (pattern' |*) <*> maybeGuardedFnBody (is "=")

fnDefOrSig :: Parser FnDefOrSig
fnDefOrSig = Def <$> fnDef <|>
             Sig <$> fnSig


fnBody :: Parser FnBody
fnBody = _ where

  fnApply = FnApply <$> fnBody <*> (fnBody |*)

  lambdaExpr = LambdaExpr <$> (is '\\' *> someSeparatedBy comma var)
                          <*> (is "->" *> fnBody)

  letExpr = LetExpr <$> (is "let" *> withinContext fnDefOrSig)
                    <*> (is "in"  *> fnBody)

  whereExpr = WhereExpr <$> fnBody <* is "where"
                        <*> withinContext fnDefOrSig

  ifExpr = IfExpr <$> (is "if"   *> fnBody)
                  <*> (is "then" *> fnBody)
                  <*> (is "else" *> fnBody)

  multiWayIfExpr = MultiWayIfExpr <$> withinContext (guardedFnBody (is "->"))

  doExpr = DoExpr <$> (is "do" *> withinContext doStep)

  caseOfExpr = CaseOfExpr <$> (is "case" *> fnBody <* is "of")
                          <*> withinContext caseBinding

  tuple = tupleOf fnBody

  list = listOf fnBody

  var' = Var' <$> var

  literal' = Literal' <$> literal




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
guard = Guard <$> (is "|" *> someSeparatedBy comma patternGuard)


patternGuard :: Parser PatternGuard
patternGuard = PatternGuard <$> (pattern' <* is "<-") <*> fnBody <|>
               SimpleGuard  <$> fnBody <|>
               Otherwise    <$ is "otherwise"


withinContext :: Parser b -> Parser [b]
withinContext parser =  withinCurlyBrackets $ someSeparatedBy (is ";") parser
