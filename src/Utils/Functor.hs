module Utils.Functor where

import ClassyPrelude

ffmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
ffmap = fmap . fmap


infixl 4 <<$>>
(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = ffmap
