module Utils.String where

import Data.Foldable.Extra (Foldable (fold))
import Data.List           (intercalate)
import Data.Monoid.HT      (when)
import Utils.Foldable      (hasSome, wrapMaybe)
import Utils.List          (safeLast)


wrap :: String -> String -> String -> String
wrap beg end x = when (hasSome x) $ beg ++ x ++ end

wrapBoth :: String -> String -> String
wrapBoth x = wrap x x

wrapParens :: String -> String
wrapParens = wrap "(" ")"

wrapSquare :: String -> String
wrapSquare = wrap "[" "]"

wrapCurly :: String -> String
wrapCurly = wrap "{" "}"

wrapSpaces :: String -> String
wrapSpaces = wrapBoth " "

wrapQuotes :: String -> String
wrapQuotes = wrapBoth "'"

wrapBackQuotes :: String -> String
wrapBackQuotes = wrapBoth "`"

wrapDoubleQuotes :: String -> String
wrapDoubleQuotes = wrapBoth "\""



wrap' :: String -> String -> String -> String
wrap' beg end x = beg ++ x ++ end

wrapBoth' :: String -> String -> String
wrapBoth' x = wrap' x x

wrapParens' :: String -> String
wrapParens' = wrap' "(" ")"

wrapSquare' :: String -> String
wrapSquare' = wrap' "[" "]"

wrapCurly' :: String -> String
wrapCurly' = wrap' "{" "}"

wrapSpaces' :: String -> String
wrapSpaces' = wrapBoth' " "

wrapQuotes' :: String -> String
wrapQuotes' = wrapBoth' "'"

wrapDoubleQuotes' :: String -> String
wrapDoubleQuotes' = wrapBoth' "\""



wrapParensCsv :: Show a => [a] -> String
wrapParensCsv = wrapParens . str ", "

wrapSquareCsv :: Show a => [a] -> String
wrapSquareCsv = wrapSquare . str ", "

wrapCurlyCsv :: Show a => [a] -> String
wrapCurlyCsv = wrapCurly . str ", "

wrapLetContext :: (Show a, Show b) => [a] -> b -> String
wrapLetContext x y = wrapContext $ wrapBoth "\n\n" (str "\n" x) ++ show y


wrapBlock :: Show a => [a] -> String
wrapBlock x = wrapContext $ wrapBoth "\n" $ str "\n" x

wrapSpacedBlock :: Show a => [a] -> String
wrapSpacedBlock x = wrapContext $ wrapBoth "\n\n" $ str "\n\n" x

wrapSingleBlock :: Show a => a -> String
wrapSingleBlock x = wrapContext $ wrapBoth "\n" $ show x

wrapContext :: String -> String
wrapContext = intercalate "\n" . (indent 2 <$>) . lines
  where
    indent n x = when (hasSome x) (replicate n ' ' ++ x)



str :: Show a => String -> [a] -> String
str sep xs = intercalate sep $ show <$> xs

strs :: [String] -> [String] -> String
strs seps xs = fold elems ++ fold (safeLast xs)
  where
    elems = do (x, sep) <- zip xs seps
               pure $ x ++ sep

joinWords :: [String] -> String
joinWords x = unwords $ filter hasSome x

joinLines :: [String] -> String
joinLines x = intercalate "\n\n\n" $ filter hasSome x

joinMaybe :: Show a => String -> Maybe a -> String
joinMaybe sep x = foldMap ((sep +++) . show) x

joinList :: Show a => String -> String -> [a] -> String
joinList start sep x = foldMap ((start +++) . str sep) (wrapMaybe x)

(+++) :: String -> String -> String
(+++) x y = x ++ " " ++ y
