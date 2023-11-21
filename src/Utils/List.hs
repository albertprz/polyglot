module Utils.List where

import ClassyPrelude

headMaybe :: MonoFoldable mono => mono -> Maybe (Element mono)
headMaybe = fmap head . fromNullable

lastMaybe :: MonoFoldable mono => mono -> Maybe (Element mono)
lastMaybe = fmap last . fromNullable

tailList :: IsSequence seq => seq -> seq
tailList = foldMap tail . fromNullable

initList :: IsSequence seq => seq -> seq
initList = foldMap tail . fromNullable


groupTuplesByKey :: Eq a => [(a, b)] -> [(a, [b])]
groupTuplesByKey =
  fmap (\a -> (fst $ head $ impureNonNull a, fmap snd a))
  . groupBy ((==) `on` fst)


zipWithKey :: Eq a => [(a, b)] ->  [(a, c)] -> [(b, Maybe c)]
zipWithKey x y = (\a -> (snd a, snd <$> find (\b -> fst a == fst b) y)) <$> x


mergeUnion :: Eq a => [(a, b)] -> [(a, c)] -> [(Maybe b, Maybe c)]
mergeUnion x y =  a <> b
  where
    a = first Just <$> zipWithKey x y
    b = tupleReverse . first Just <$> zipWithKey y x


mix :: [a] -> [a] -> [a]
mix (x : xs) (y : ys) = x : y : mix xs ys
mix [] ys             = ys
mix xs []             = xs


tupleReverse :: (b, a) -> (a, b)
tupleReverse (x, y) = (y, x)
