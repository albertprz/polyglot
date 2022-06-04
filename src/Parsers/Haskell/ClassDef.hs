module Parsers.Haskell.ClassDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef (ClassDef),
                                     InstanceDef (InstanceDef))

import Data.Maybe             (maybeToList)
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

  classConstraints' = mconcat . maybeToList <$>
                     ((classConstraints type' <* is "=>") |?)

  typeParams = mconcat . maybeToList <$>
               ((tupleOf typeParam <|> (: []) <$> typeParam) |?)



instanceDef :: Parser InstanceDef
instanceDef = InstanceDef <$> (is "class" *> classConstraints')
                          <*> class'
                          <*> types
                          <* is "where"
                          <*> withinContext fnDefOrSig
  where

  classConstraints' = mconcat . maybeToList <$>
                     ((classConstraints type' <* is "=>") |?)

  types = mconcat . maybeToList <$>
               ((tupleOf anyKindedType <|> (: []) <$> anyKindedType) |?)
