module Conversions.HaskellToScala.Pattern where

import qualified SyntaxTrees.Haskell.Common  as H
import qualified SyntaxTrees.Haskell.Pattern as H
import qualified SyntaxTrees.Scala.Common    as S
import qualified SyntaxTrees.Scala.Pattern   as S

import Conversions.HaskellToScala.Common


pattern' :: H.Pattern -> S.Pattern
pattern' (H.CtorPattern x y) = S.CtorPattern (ctor x) (pattern' <$> y)
pattern' (H.InfixCtorPattern x y) = S.InfixCtorPattern (ctorOp x) (pattern' <$> y)
pattern' (H.RecordPattern x y) = S.RecordPattern (ctor x) (namedFieldFn <$> y)
pattern' (H.WildcardRecordPattern x y) = S.WildcardRecordPattern (ctor x)
                                          (namedFieldFn <$> y)
pattern' (H.AliasedPattern x y) = S.AliasedPattern (var x) (pattern' y)
pattern' (H.ListPattern x) = S.InfixCtorPattern  (S.CtorOp "::") (pattern' <$> x)
pattern' (H.TuplePattern x) = S.TuplePattern $ pattern' <$> x
pattern' (H.VarPattern x) = S.VarPattern $ var x
pattern' (H.LitPattern x) = S.LitPattern $ literal x
pattern' H.Wildcard = S.Wildcard


namedFieldFn :: (H.Var, Maybe H.Pattern) -> (S.Var, Maybe S.Pattern)
namedFieldFn (x, y) = (var x, pattern' <$> y)
