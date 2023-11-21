module Utils.Foldable where

import ClassyPrelude

import Data.Type.Equality

hasNone :: MonoFoldable mono => mono -> Bool
hasNone = null

hasSome :: MonoFoldable mono => mono -> Bool
hasSome = not . hasNone

wrapMaybe :: MonoFoldable mono => mono -> Maybe mono
wrapMaybe x = if hasSome x then Just x else Nothing

andPred :: (Element mono ~ (p -> Bool), MonoFoldable mono) => mono -> p -> Bool
andPred ps a = all ($ a) ps

orPred :: (Element mono ~ (p -> Bool), MonoFoldable mono) => mono -> p -> Bool
orPred ps a = any ($ a) ps
