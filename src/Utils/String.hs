module Utils.String where

import Data.List      (intercalate)
import Data.Monoid.HT (when)
import Utils.Foldable (hasSome, wrapMaybe)


wrap :: String -> String -> String -> String
wrap beg end x = when (hasSome x) beg ++ x ++ end

wrapBoth :: String -> String -> String
wrapBoth x = wrap x x

wrapParens :: String -> String
wrapParens = wrap "(" ")"

wrapSquare :: String -> String
wrapSquare = wrap "[" "]"

wrapCurly :: String -> String
wrapCurly = wrap "{" "}"

wrapSpaces :: String -> String
wrapSpaces = wrap " " " "


wrapParensCsv :: Show a => [a] -> String
wrapParensCsv = wrapParens . str ", "

wrapSquareCsv :: Show a => [a] -> String
wrapSquareCsv = wrapSquare . str ", "

wrapCurlyCsv :: Show a => [a] -> String
wrapCurlyCsv = wrapCurly . str ", "

wrapLetContext :: (Show a, Show b) => [a] -> b -> String
wrapLetContext x y = wrapCurly $ wrapBoth "\n\n" (str "\n" x) ++ show y

wrapBlock :: Show a => [a] -> String
wrapBlock x = wrapCurly $ wrapBoth "\n" $ str "\n" x

wrapSpacedBlock :: Show a => [a] -> String
wrapSpacedBlock x = wrapCurly $ wrapBoth "\n\n" $ str "\n\n" x



str :: Show a => String -> [a] -> String
str sep x = intercalate sep $ show <$> x

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
