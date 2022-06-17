module Utils.Functor where

ffmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
ffmap x y = (fmap . fmap) x y

infixl 4 <$$>
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = ffmap
