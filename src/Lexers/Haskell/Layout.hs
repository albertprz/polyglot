module Lexers.Haskell.Layout where


import Parser
import ParserCombinators
import Parsers.Char

import Control.Monad    (foldM)
import Data.Foldable    (Foldable (fold))
import Data.Monoid.HT   (when)
import Data.Tuple.Extra (fst3)
import Parsers.String   (word)
import Utils.Foldable   (hasNone, hasSome)
import Utils.List       (safeHead)


layoutBegin :: Parser String
layoutBegin = oneOf layoutTokens


otherText :: Parser String
otherText = mconcat <$>
           (((word <&> noneOf layoutTokens) >>> (space |*)) |*)



ammendLayout :: String -> Either ParseError String
ammendLayout str = unlines . fst3 <$> foldM layout args (lines str)
  where
    args = ([], [0], False)


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
         let endSep = when (hasSome layoutText) "{"
         let indents' = when (hasSome rest) [contextIndent] ++ newIndents
         let text = x ++ [spaces' ++ beginSep ++ start ++ fold layoutText ++
                          endSep ++  spaces'' ++ rest]
         pure (text, indents', layoutNextLine)



calcIndent :: [Int] -> Int -> ([Int], String)
calcIndent indentLvls curr = (newIndentLvls, closeContexts ++ sep)
  where
    closeContexts = mconcat ("}" <$ extra)
    sep = when (all (== curr) (safeHead indentLvls)) ";"
    (extra, newIndentLvls) = span (curr <) indentLvls


layoutTokens :: [String]
layoutTokens = ["where", "let", "do", "of"]
