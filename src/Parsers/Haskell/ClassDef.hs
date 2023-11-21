module Parsers.Haskell.ClassDef where

import ClassyPrelude

import SyntaxTrees.Haskell.ClassDef (ClassDef (..), DerivingDef (..),
                                     InstanceDef (..))
import SyntaxTrees.Haskell.Type     (ClassConstraint)

import Parsers.Haskell.Common (class')
import Parsers.Haskell.FnDef  (betweenContext, fnDefOrSig)
import Parsers.Haskell.Type   (anyKindedType, classConstraints, type',
                               typeParam)

import Bookhound.Parser            (Parser, withError)
import Bookhound.ParserCombinators (string, (|*), (|+), (|?))

import SyntaxTrees.Haskell.DataDef (DerivingStrategy (..))


classDef :: Parser ClassDef
classDef = withError "Class declaration" $
  ClassDef <$> (string "class" *> classConstraints')
           <*> class'
           <*> (typeParam |*)
           <* string "where"
           <*> betweenContext fnDefOrSig


instanceDef :: Parser InstanceDef
instanceDef = withError "Instance declaration" $
  InstanceDef <$> (string "instance" *> classConstraints')
              <*> class'
              <*> (anyKindedType |+)
              <* string "where"
              <*> betweenContext fnDefOrSig

derivingDef :: Parser DerivingDef
derivingDef = withError "Standalone deriving declaration" $
  DerivingDef <$> (string "deriving" *>
                   derivingStrategy <* string "instance")
              <*> classConstraints'
              <*> class'
              <*> (anyKindedType |+)
              <*> ((string "via" *> class') |?)

derivingStrategy :: Parser DerivingStrategy
derivingStrategy = (StandardDeriving <$ string "stock")
                   <|> (NewTypeDeriving <$ string "newtype")
                   <|> (AnyClassDeriving <$ string "anyclass")
                   <|> pure StandardDeriving


classConstraints' :: Parser [ClassConstraint]
classConstraints' = fold <$>
                    ((classConstraints type' <* string "=>") |?)
