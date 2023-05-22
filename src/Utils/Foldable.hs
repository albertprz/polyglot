module Utils.Foldable where


hasNone :: Foldable t => t a -> Bool
hasNone = null

hasSome :: Foldable t => t a -> Bool
hasSome = not . hasNone

wrapMaybe :: Foldable t => t a -> Maybe (t a)
wrapMaybe x = if hasSome x then Just x else Nothing

orPred :: (Foldable t, Functor t) => t (p -> Bool) -> p -> Bool
orPred ps a = or $ ($ a) <$> ps

andPred :: (Foldable t, Functor t) => t (p -> Bool) -> p -> Bool
andPred ps a = and $  ($ a) <$> ps
