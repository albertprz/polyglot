module Utils.Foldable where

import Data.Foldable (Foldable (toList))
import Utils.List    (safeTail)

hasNone :: Foldable m => m a -> Bool
hasNone = null

hasSome :: Foldable m => m a -> Bool
hasSome = not . hasNone

hasMany :: Foldable m => m a -> Bool
hasMany = all hasSome . safeTail . toList

wrapMaybe :: Foldable t => t a -> Maybe (t a)
wrapMaybe x = if hasSome x then Just x else Nothing
