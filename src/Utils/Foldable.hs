module Utils.Foldable where

import Data.Foldable (Foldable (toList))
import Utils.List    (safeTail)

hasNone :: Foldable t => t a -> Bool
hasNone = null

hasSome :: Foldable t => t a -> Bool
hasSome = not . hasNone

hasMany :: Foldable t => t a -> Bool
hasMany = all hasSome . safeTail . toList

wrapMaybe :: Foldable t => t a -> Maybe (t a)
wrapMaybe x = if hasSome x then Just x else Nothing

orPred :: (Foldable t, Functor t) => t (p -> Bool) -> p -> Bool
orPred ps a = or $ ($ a) <$> ps

andPred :: (Foldable t, Functor t) => t (p -> Bool) -> p -> Bool
andPred ps a = and $  ($ a) <$> ps
