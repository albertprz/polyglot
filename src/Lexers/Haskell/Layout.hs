module Lexers.Haskell.Layout where


import Parsers.Haskell.Common (anyComment)

import Utils.Foldable (hasNone, hasSome)
import Utils.List     (safeHead, safeTail)
import Utils.String   (overText, wrapCurly', wrapDoubleQuotes', wrapParens',
                       wrapQuotes')


import Bookhound.Parser            (ParseError, Parser, anyChar, runParser,
                                    satisfy, withError)
import Bookhound.ParserCombinators (IsMatch (is, isNot, noneOf, oneOf), (->>-),
                                    (|*), (|+), (|?), (||*), (||+))
import Bookhound.Parsers.Char      (space)
import Bookhound.Parsers.Text      (betweenDoubleQuotes, betweenParens,
                                    betweenQuotes, spacing, word)


import           Control.Applicative ((<|>))
import           Control.Monad       (foldM)
import           Data.Foldable       (Foldable (fold))
import           Data.Foldable.Extra (sum')
import           Data.Monoid.HT      (when)
import           Data.Text           (Text, pack)
import qualified Data.Text           as Text


adaptLayout :: Text -> Either [ParseError] Text
adaptLayout str = (<> " }") . Text.unlines . fst4 <$> layoutLines
  where
    layoutLines = foldM layout args . (<> pure "") . filter (not . Text.null) =<< input
    input = Text.lines . fold <$> runParser parensLayout str
    args = ([], [], False, False)
    fst4 (x, _, _, _) = x


layout :: ([Text], [Int], Bool, Bool) -> Text -> Either [ParseError] ([Text], [Int], Bool, Bool)
layout (x, y, z, t) = runParser layoutParser
  where
    layoutParser = withError "Layout lexer" $
      do spaces' <- (space ||*)
         beginning <- otherText
         layoutText <- (layoutBegin |?)
         spaces'' <- (space ||*)
         rest <- otherText
         let hasIn = Just "in" == safeHead (Text.words beginning)
             hasCurly = Just '{' == fmap fst (Text.uncons rest)
             indents = when z [Text.length spaces'] <>
                if not hasIn then y
                else (Text.length spaces' + 1) : fold (safeTail y)
             layoutNextLine = hasSome layoutText && Text.null rest
             contextIndent = sum' $ Text.length <$>
                [spaces', beginning, fold layoutText, spaces'']
             (newIndents, beginSep, stop) =
                 calcIndent indents (Text.length spaces')
                                            (t || hasCurly) beginning
             endSep = when (hasSome layoutText && not hasCurly) " {"
             indents' = when (hasSome layoutText && not (Text.null rest))
                        [contextIndent] <> newIndents
             text = x <> [spaces' <> beginSep <> beginning <> fold
                          layoutText <> endSep <>  spaces'' <> rest]
         pure (text, indents', layoutNextLine, stop || hasCurly)


parensLayout :: Parser [Text]
parensLayout = (((spacing |?) ->>- elem'
               <|> parensParser
               <|> (overText wrapParens' . fold <$> betweenParens parensLayout) ->>-
                   (spacing |?)) |*)
  where
    elem' = lexeme' id
    parensParser = overText wrapParens' <$> betweenParens
                ((spacing |?) ->>-
                 (layoutBegin <> spacing <>
                 (overText wrapCurly' . fold <$> parensLayout)))



calcIndent :: [Int] -> Int -> Bool -> Text -> ([Int], Text, Bool)
calcIndent indentLvls curr stop beginning =
  (newIndentLvls, joinWords [closeContexts, sep], shouldStop)
  where
    extraElems = if not stop then extra else fold $ safeTail extra
    closeContexts = fold ("} " <$ extraElems)
    shouldStop = stop && hasNone extra
    sep = when (elem curr (safeHead newIndentLvls) &&
                notElem beginning nestTokens) "; "
    (extra, newIndentLvls) = span (curr <) indentLvls
    joinWords = Text.unwords . filter (not . Text.null)


nestTokens :: [Text]
nestTokens = ["then", "else"]

layoutTokens :: [Text]
layoutTokens = [("(" <>), id] <*> ["where", "let", "do", "of", "\\case"]

layoutBegin :: Parser Text
layoutBegin = oneOf layoutTokens

lexeme :: Parser Text
lexeme = pack . wrapDoubleQuotes' <$> betweenDoubleQuotes (isNot '"' |+)
  <|> pack . wrapQuotes' . pure
      <$> betweenQuotes (anyChar <|> (is '\\' |?) *> anyChar)
  <|> anyComment
  <|> word



otherText :: Parser Text
otherText = fold <$> elems
  where
    elems = ((satisfy (`notElem` layoutTokens) lexeme <> (space ||*)) |*)


lexeme' :: (Parser Text -> Parser Text) -> Parser Text
lexeme' f = (spacing |?) ->>- f lexeme ->>- (spacing |?)

otherText' :: Parser Text
otherText' = lexeme' (satisfy (`notElem` layoutTokens))


word' :: Parser Text
word' = (noneOf [' ', '\n', '\t', '(', ')'] ||+)
