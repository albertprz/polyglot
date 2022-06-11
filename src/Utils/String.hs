module Utils.String where

import Data.List      (intercalate)
import Utils.Foldable (toMaybe)


wrapParens :: String -> String
wrapParens x = if (not . null) x then "(" ++ show x ++ ")" else ""

wrapSquare :: String -> String
wrapSquare x = if (not . null) x then "[" ++ x ++ "]" else ""

wrapCurly :: String -> String
wrapCurly x = if (not . null) x then "{" ++ x ++ "}" else ""

wrapSpaces :: String -> String
wrapSpaces x = if (not . null) x then " " ++ x ++ " " else ""


wrapParensCsv :: Show a => [a] -> String
wrapParensCsv = wrapParens . str ", "

wrapSquareCsv :: Show a => [a] -> String
wrapSquareCsv = wrapSquare . str ", "

wrapCurlyCsv :: Show a => [a] -> String
wrapCurlyCsv = wrapCurly . str ", "

wrapLetContext :: (Show a, Show b) => [a] -> b -> String
wrapLetContext x y = wrapCurly ("\n" ++ str "\n" x ++ "\n\n" ++ show y)

wrapBlock :: Show a => [a] -> String
wrapBlock x = wrapCurly $ "\n" ++ str "\n" x

wrapSpacedBlock :: Show a => [a] -> String
wrapSpacedBlock x = wrapCurly $ "\n\n" ++ str "\n\n" x





str :: Show a => String -> [a] -> String
str sep x = intercalate sep $ show <$> x

joinWords :: [String] -> String
joinWords x = unwords $ filter (not . null) x

joinLines :: [String] -> String
joinLines x = intercalate "\n\n\n" $ filter (not . null) x

joinMaybe :: Show a => String -> Maybe a -> String
joinMaybe sep x = maybe "" ((sep +++) . show) x

joinList :: Show a => String -> String -> [a] -> String
joinList start sep x = maybe "" ((start +++) . (str sep)) (toMaybe x)

(+++) :: String -> String -> String
(+++) x y = x ++ " " ++ y
