module Utils.Maybe where

import Data.List.Extra (snoc)

cond :: Bool -> a -> Maybe a
cond x y = if x then Just y else Nothing

msnoc :: [a] -> Maybe a -> [a]
msnoc xs y = maybe xs (snoc xs) y
