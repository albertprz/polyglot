module Conversions.ToPurescript.Pattern where

import qualified SyntaxTrees.Haskell.Pattern    as H
import qualified SyntaxTrees.Purescript.Common  as P
import qualified SyntaxTrees.Purescript.Pattern as P

import Conversions.ToPurescript.Common


pattern' :: H.Pattern -> P.Pattern
pattern' (H.CtorPattern x y) = P.CtorPattern (qCtor x) (pattern' <$> y)
pattern' (H.InfixCtorPattern x y) = P.InfixCtorPattern (qCtorOp x)
                                                       (pattern' <$> y)
pattern' (H.RecordPattern x y) = P.RecordPattern (qCtor x)
                                ((\(z, t) -> (var z, pattern' <$> t)) <$> y)
pattern' (H.WildcardRecordPattern x y) = pattern' $ H.RecordPattern x y
pattern' (H.AliasedPattern x y) = P.AliasedPattern (var x) (pattern' y)
pattern' (H.ListPattern x) = P.CtorPattern
  (P.QCtor Nothing $ P.Ctor "Array") (pattern' <$> x)
pattern' (H.TuplePattern x) = P.TuplePattern $ pattern' <$> x
pattern' (H.VarPattern x) = P.VarPattern $ var x
pattern' (H.LitPattern x) = P.LitPattern $ literal x
pattern' H.Wildcard = P.Wildcard
