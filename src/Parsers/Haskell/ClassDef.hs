module Parsers.Haskell.ClassDef where

import SyntaxTrees.Haskell.ClassDef (ClassDef (..), DerivingDef (..),
                                     InstanceDef (..))
import SyntaxTrees.Haskell.Type     (ClassConstraint)

import Parsers.Haskell.Common (class')
import Parsers.Haskell.FnDef  (fnDefOrSig, withinContext)
import Parsers.Haskell.Type   (anyKindedType, classConstraints, type',
                               typeParam)

import Bookhound.Parser            (Parser, withError)
import Bookhound.ParserCombinators (IsMatch (is), (<|>), (|*), (|+), (|?))

import Data.Foldable               (Foldable (fold))
import SyntaxTrees.Haskell.DataDef (DerivingStrategy (..))


classDef :: Parser ClassDef
classDef = withError "Class declaration" $
  ClassDef <$> (is "class" *> classConstraints')
           <*> class'
           <*> (typeParam |*)
           <* is "where"
           <*> withinContext fnDefOrSig


instanceDef :: Parser InstanceDef
instanceDef = withError "Instance declaration" $
  InstanceDef <$> (is "instance" *> classConstraints')
              <*> class'
              <*> (anyKindedType |+)
              <* is "where"
              <*> withinContext fnDefOrSig

derivingDef :: Parser DerivingDef
derivingDef = withError "Standalone deriving declaration" $
  DerivingDef <$> (is "deriving" *>
                   derivingStrategy <* is "instance")
              <*> classConstraints'
              <*> class'
              <*> (anyKindedType |+)
              <*> ((is "via" *> class') |?)

derivingStrategy :: Parser DerivingStrategy
derivingStrategy = (StandardDeriving <$ is "stock")
                   <|> (NewTypeDeriving <$ is "newtype")
                   <|> (AnyClassDeriving <$ is "anyclass")
                   <|> pure StandardDeriving


classConstraints' :: Parser [ClassConstraint]
classConstraints' = fold <$>
                    ((classConstraints type' <* is "=>") |?)
