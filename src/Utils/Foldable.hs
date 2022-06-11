module Utils.Foldable where


toMaybe :: Foldable t => t a -> Maybe (t a)
toMaybe x = if (not . null) x then Just x else Nothing
