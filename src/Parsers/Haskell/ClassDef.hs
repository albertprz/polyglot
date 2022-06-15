module Parsers.Haskell.ClassDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef (ClassDef),
                                     InstanceDef (InstanceDef))

import Data.Foldable          (Foldable (fold))
import Parser                 (Parser)
import ParserCombinators      (IsMatch (is), (<|>), (|?))
import Parsers.Collections    (tupleOf)
import Parsers.Haskell.Common (class')
import Parsers.Haskell.FnDef  (fnDefOrSig, withinContext)
import Parsers.Haskell.Type   (anyKindedType, classConstraints, type',
                               typeParam)



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
instanceDef = InstanceDef <$> (is "class" *> classConstraints')
                          <*> class'
                          <*> types
                          <* is "where"
                          <*> withinContext fnDefOrSig
  where

  classConstraints' = fold <$>
                      ((classConstraints type' <* is "=>") |?)

  types = fold <$>
          ((tupleOf anyKindedType <|> pure <$> anyKindedType) |?)
