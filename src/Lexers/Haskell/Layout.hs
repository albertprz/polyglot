module Lexers.Haskell.Layout where


import Parsers.Haskell.Common (anyComment)

import Utils.Foldable (hasNone, hasSome)
import Utils.List     (safeHead, safeTail)
import Utils.String   (joinWords, wrapCurly', wrapDoubleQuotes', wrapParens',
                       wrapQuotes')


import Bookhound.Parser            (ParseError, Parser, check, runParser,
                                    withError)
import Bookhound.ParserCombinators (IsMatch (is, isNot, noneOf, oneOf), (->>-),
                                    (<|>), (|*), (|+), (|?))
import Bookhound.Parsers.Char      (char, space)
import Bookhound.Parsers.String    (spacing, withinDoubleQuotes, withinParens,
                                    withinQuotes, word)


import Control.Monad  (foldM)
import Data.Foldable  (Foldable (fold))
import Data.List      (isPrefixOf)
import Data.Monoid.HT (when)
import Data.Text      (Text, pack)


adaptLayout :: Text -> Either [ParseError] Text
adaptLayout str = pack . (++ " }") . unlines . fst4 <$> layoutLines
  where
    layoutLines = foldM layout args . (++ pure "") . filter hasSome =<< input
    input = lines . fold <$> runParser parensLayout str
    args = ([], [], False, False)
    fst4 (x, _, _, _) = x


layout :: ([String], [Int], Bool, Bool) -> String -> Either [ParseError] ([String], [Int], Bool, Bool)
layout (x, y, z, t) str = runParser layoutParser $ pack str
  where
    layoutParser = withError "Layout lexer" $
      do spaces' <- (space |*)
         beginning <- otherText
         layoutText <- (layoutBegin |?)
         spaces'' <- (space |*)
         rest <- otherText
         let hasIn = maybe False ("in" ==) (safeHead $ words beginning)
             hasCurly = isPrefixOf "{" rest
             indents = when z [length spaces'] ++
                if not hasIn then y
                else (length spaces' + 1) : (fold $ safeTail y)
             layoutNextLine = hasSome layoutText && hasNone rest
             contextIndent = length $ spaces' ++ beginning ++ fold layoutText ++ spaces''
             (newIndents, beginSep, stop) = calcIndent indents (length spaces')
                                            (t || hasCurly) beginning
             endSep = when (hasSome layoutText && not hasCurly) " {"
             indents' = when (hasSome layoutText && hasSome rest)
                        [contextIndent] ++ newIndents
             text = x ++ [spaces' ++ beginSep ++ beginning ++ fold
                          layoutText ++ endSep ++  spaces'' ++ rest]
         pure $ (text, indents', layoutNextLine, stop || hasCurly)


parensLayout :: Parser [String]
parensLayout = (((spacing |?) ->>- elem'
               <|> parensParser
               <|> (wrapParens' . fold <$> withinParens parensLayout) ->>-
                   (spacing |?)) |*)
  where
    elem' = lexeme' id
    parensParser = wrapParens' <$> withinParens
                ((spacing |?) ->>- layoutBegin ->>- spacing ->>-
                 (wrapCurly' . fold <$> parensLayout))



calcIndent :: [Int] -> Int -> Bool -> String -> ([Int], String, Bool)
calcIndent indentLvls curr stop beginning =
  (newIndentLvls, joinWords [closeContexts, sep], shouldStop)
  where
    extraElems = if not stop then extra else fold $ safeTail extra
    closeContexts = fold ("} " <$ extraElems)
    shouldStop = stop && hasNone extra
    sep = when (any (== curr) (safeHead newIndentLvls) &&
                notElem beginning nestTokens) "; "
    (extra, newIndentLvls) = span (curr <) indentLvls


nestTokens :: [String]
nestTokens = ["then", "else"]

layoutTokens :: [String]
layoutTokens = [("(" ++), id] <*> ["where", "let", "do", "of", "\\case"]

layoutBegin :: Parser String
layoutBegin = oneOf layoutTokens

lexeme :: Parser String
lexeme = wrapDoubleQuotes' <$> withinDoubleQuotes (isNot '"'  |+)
  <|> wrapQuotes' . pure <$> withinQuotes (char <|> (is '\\' |?) *> char)
  <|> anyComment
  <|> word



otherText :: Parser String
otherText = fold <$> elems
  where
    elems = (((check "" (`notElem` layoutTokens) lexeme) ->>- (space |*)) |*)


lexeme' :: (Parser String -> Parser String) -> Parser String
lexeme' f = (spacing |?) ->>- f lexeme ->>- (spacing |?)

otherText' :: Parser String
otherText' = lexeme' (check "" (`notElem` layoutTokens))


word' :: Parser String
word' = ((noneOf [' ', '\n', '\t', '(', ')']) |+)
