module Lexers.Haskell.Layout where


import Parser            (ParseError, Parser, check, runParser)
import ParserCombinators (IsMatch (is, isNot, oneOf), (<|>), (>>>), (|*), (|?))
import Parsers.Char      (char, space)

import Control.Monad    (foldM)
import Data.Foldable    (Foldable (fold))
import Data.Monoid.HT   (when)
import Data.Tuple.Extra (fst3)
import Parsers.String   (withinDoubleQuotes, withinQuotes, word)
import Utils.Foldable   (hasNone, hasSome)
import Utils.List       (safeHead)
import Utils.String     (joinWords, wrapDoubleQuotes, wrapQuotes)



adaptLayout :: String -> Either ParseError String
adaptLayout str = (++ "}") . unlines . fst3 <$> layoutLines
  where
    layoutLines = foldM layout args (filter (/= "") (lines str) ++ pure "")
    args = ([], [], False)


layout :: ([String], [Int], Bool)
          -> String
          -> Either ParseError ([String], [Int], Bool)
layout (x, y, z) str = runParser layoutParser str
  where
    layoutParser =
      do spaces' <- (space |*)
         start <- otherText
         layoutText <- (layoutBegin |?)
         spaces'' <- (space |*)
         rest <- otherText
         let indents = when z [length spaces'] ++ y
         let layoutNextLine = hasSome layoutText && hasNone rest
         let contextIndent = length $ spaces' ++ start ++ fold layoutText ++ spaces''
         let (newIndents, beginSep) = calcIndent indents $ length spaces'
         let endSep = when (hasSome layoutText) " {"
         let indents' = when (hasSome layoutText && hasSome rest)
                        [contextIndent] ++ newIndents
         let text = x ++ [spaces' ++ beginSep ++ start ++ fold layoutText ++
                          endSep ++  spaces'' ++ rest]
         pure $ (text, indents', layoutNextLine)



calcIndent :: [Int] -> Int -> ([Int], String)
calcIndent indentLvls curr = (newIndentLvls, joinWords [closeContexts, sep])
  where
    closeContexts = fold ("}; " <$ extra)
    sep = when (any (== curr) (safeHead indentLvls)) "; "
    (extra, newIndentLvls) = span (curr <) indentLvls


layoutTokens :: [String]
layoutTokens = ["where", "let", "do", "of", "\\case", "(\\case"]

layoutBegin :: Parser String
layoutBegin = oneOf layoutTokens


otherText :: Parser String
otherText = mconcat <$>
           (((check "" (`notElem` layoutTokens) lexeme) >>> (space |*)) |*)

lexeme :: Parser String
lexeme = wrapDoubleQuotes  <$> withinDoubleQuotes (isNot '"'  |*) <|>
         wrapQuotes . pure <$> withinQuotes (char <|> ((is '\\' |?) *> char)) <|>
         word
