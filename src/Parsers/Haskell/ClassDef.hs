module Parsers.Haskell.ClassDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef (ClassDef),
                                     InstanceDef (InstanceDef))

import Parsers.Haskell.Common (class')
import Parsers.Haskell.FnDef  (fnDefOrSig, withinContext)
import Parsers.Haskell.Type   (anyKindedType, classConstraints, type',
                               typeParam)

import Bookhound.Parser              (Parser)
import Bookhound.ParserCombinators   (IsMatch (is), (<|>), (|?))
import Bookhound.Parsers.Collections (tupleOf)

import Data.Foldable (Foldable (fold))


classDef :: Parser ClassDef
classDef = ClassDef <$> (is "class" *> classConstraints')
                    <*> class'
                    <*> typeParams
                    <* is "where"
                    <*> withinContext fnDefOrSig
  where

  classConstraints' = fold <$>
                      ((classConstraints type' <* is "=>") |?)

  typeParams = fold <$>
               ((tupleOf typeParam <|> pure <$> typeParam) |?)



instanceDef :: Parser InstanceDef
instanceDef = InstanceDef <$> (is "instance" *> classConstraints')
                          <*> class'
                          <*> types
                          <* is "where"
                          <*> withinContext fnDefOrSig
  where

  classConstraints' = fold <$>
                      ((classConstraints type' <* is "=>") |?)

  types = fold <$>
          ((tupleOf anyKindedType <|> pure <$> anyKindedType) |?)
