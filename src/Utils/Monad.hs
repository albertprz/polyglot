module Utils.Monad where

(>>.) :: Monad m => (a1 -> m a2) -> (a2 -> m b) -> a1 -> m b
(>>.) x y = \i -> pure i >>= x >>= y
