module Conversions.ToPurescript.Pattern where

import qualified SyntaxTrees.Haskell.Pattern    as H
import qualified SyntaxTrees.Purescript.Pattern as P

import Conversions.ToPurescript.Common (literal, qCtor, qCtorOp, var)
import Conversions.ToPurescript.Type   (type')


pattern' :: H.Pattern -> P.Pattern
pattern' (H.CtorPattern x y) =
  P.CtorPattern (qCtor x) (pattern' <$> y)
pattern' (H.InfixCtorPattern x y) =
  P.InfixCtorPattern (qCtorOp x) (pattern' <$> y)
pattern' (H.RecordPattern x y) =
  P.RecordPattern (qCtor x) ((\(z, t) -> (var z, pattern' <$> t)) <$> y)
pattern' (H.WildcardRecordPattern x y) =
  pattern' $ H.RecordPattern x y
pattern' (H.AliasedPattern x y) =
  P.AliasedPattern (var x) (pattern' y)
pattern' (H.TypeAnnotation x y) =
  P.TypeAnnotation (pattern' x) (type' y)
pattern' (H.ListPattern x) = P.ArrayPattern (pattern' <$> x)
pattern' (H.TuplePattern x) = P.TuplePattern $ pattern' <$> x
pattern' (H.VarPattern x) = P.VarPattern $ var x
pattern' (H.LitPattern x) = P.LitPattern $ literal x
pattern' H.Wildcard = P.Wildcard
