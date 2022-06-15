module Utils.List where

import qualified Data.List.NonEmpty as Nel

import Data.Foldable    (find)
import Data.List        (groupBy, sortBy)
import Data.Tuple.Extra (first)



safeHead :: [a] -> Maybe a
safeHead = (Nel.head <$>) . Nel.nonEmpty

safeLast :: [a] -> Maybe a
safeLast = (Nel.last <$>) . Nel.nonEmpty

safeTail :: [a] -> Maybe [a]
safeTail = (Nel.tail <$>) . Nel.nonEmpty

safeInit :: [a] -> Maybe [a]
safeInit = (Nel.init <$>) . Nel.nonEmpty


groupByKey :: Eq b => (a -> b) -> [a] -> [[a]]
groupByKey f x = groupBy (\a b -> f a == f b) x


groupTuplesByKey :: Eq a => [(a, b)] -> [(a, [b])]
groupTuplesByKey x = (\a -> (fst $ head a, snd <$> a)) <$>
                        groupByKey fst x


sortByKey :: Ord b => (a -> b) -> [a] -> [a]
sortByKey f x = sortBy (\a b -> f a `compare` f b) x


zipWithKey :: Eq a => [(a, b)] ->  [(a, c)] -> [(b, Maybe c)]
zipWithKey x y = (\a ->  (snd a, snd <$> (find (\b -> fst a == fst b) y))) <$> x

mergeUnion :: (Eq a) => [(a, b)] -> [(a, c)] -> [(Maybe b, Maybe c)]
mergeUnion x y =  a ++ b
  where
    a = (first Just) <$> zipWithKey x y
    b = (tupleReverse . (first Just)) <$> zipWithKey y x




tupleReverse :: (b, a) -> (a, b)
tupleReverse (x, y) = (y, x)
