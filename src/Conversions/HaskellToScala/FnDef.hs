module Conversions.HaskellToScala.FnDef where

import qualified SyntaxTrees.Haskell.Common as H
import qualified SyntaxTrees.Haskell.FnDef  as H
import qualified SyntaxTrees.Haskell.Type   as H
import qualified SyntaxTrees.Scala.Common   as S
import qualified SyntaxTrees.Scala.FnDef    as S
import qualified SyntaxTrees.Scala.Type     as S

import           Conversions.HaskellToScala.Common  (var)
import           Conversions.HaskellToScala.Pattern (pattern')
import           Conversions.HaskellToScala.Type    (findTypeParams, type',
                                                     typeParam, typeSplit)
import           Data.Foldable                      (Foldable (toList))
import qualified SyntaxTrees.Scala.Pattern          as S


emptyFnSig :: S.FnSig
emptyFnSig = S.FnSig [] [] (S.UsingArgList []) Nothing

namedFnSig :: H.Type -> [H.Var] -> S.FnSig
namedFnSig tpe args = S.FnSig (typeParam <$> (toList $ findTypeParams tpe))
        [S.ArgList $ uncurry (S.ArgField []) <$> zip (var <$> args) argTypes]
        (S.UsingArgList []) (Just retType)
 where
   (argTypes, retType) = typeSplit (length args) tpe


unNamedFnSig :: H.Type -> Int -> S.FnSig
unNamedFnSig tpe n = S.FnSig (typeParam <$> (toList $ findTypeParams tpe))
        [S.ArgList $ uncurry (S.ArgField []) <$> zip (S.Var <$> autoFnIds) argTypes]
        (S.UsingArgList []) (Just retType)
 where
   (argTypes, retType) = typeSplit n tpe

fnDef :: H.FnSig -> [H.FnDef] -> S.MethodDef
fnDef (H.FnSig x y) defs = S.MethodDef [] (var x) (unNamedFnSig y n)
                                       (Just $ _ defs)
  where
    n = (length . (.args)) (head defs)

simpleFnDef :: H.FnSig -> H.FnDef -> [H.Var] -> S.MethodDef
simpleFnDef (H.FnSig x y) def args = S.MethodDef [] (var x) (namedFnSig y args)
                                                 (Just $ _ $ pure def)

internalFnDef :: [H.FnDef] -> S.MethodDef
internalFnDef defs = S.MethodDef [] (var name) emptyFnSig
                                 (Just $ fnDefToFnBody defs)
  where
    name = (.name) (head defs)


simpleFnDefToFnBody :: H.FnDef -> S.MaybeGuardedFnBody
simpleFnDefToFnBody def = maybeGuardeBody . (.body) $ def

fnDefToFnBody :: [H.FnDef] -> S.FnBody
fnDefToFnBody defs = match
  where
    match = S.MatchExpr (tuple $ take n autoFnIds) cases
    casePatterns = (S.TuplePattern  . (pattern' <$>) . (.args)) <$> defs
    caseBodies =  (maybeGuardeBody . (.body)) <$> defs
    cases = uncurry S.CaseBinding <$> zip casePatterns caseBodies
    tuple x = S.Tuple $ (S.FnVar' . S.Var' . S.Var) <$> x
    n = (length . (.args)) (head defs)

maybeGuardeBody :: H.MaybeGuardedFnBody -> S.MaybeGuardedFnBody
maybeGuardeBody x = _

autoFnIds :: [String]
autoFnIds = pure <$> ['x', 'y', 'z', 't', 'u', 'v', 'w', 'p', 'q', 'r', 's']
