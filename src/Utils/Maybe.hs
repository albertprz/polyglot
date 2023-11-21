module Utils.Maybe where

import ClassyPrelude

cond :: Bool -> a -> Maybe a
cond x y = if x then Just y else Nothing
