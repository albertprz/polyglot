module Conversions.HaskellToScala.FnDef where

import qualified SyntaxTrees.Haskell.Common as H
import qualified SyntaxTrees.Haskell.FnDef  as H
import qualified SyntaxTrees.Haskell.Type   as H
import qualified SyntaxTrees.Scala.Common   as S
import qualified SyntaxTrees.Scala.FnDef    as S
import qualified SyntaxTrees.Scala.Pattern  as S

import Conversions.HaskellToScala.Common  (literal, var, varOp)
import Conversions.HaskellToScala.Pattern (allVars, extractVars, pattern')
import Conversions.HaskellToScala.Type    (argList, classScopeSplit,
                                           findTypeParams, fnOp, typeParam,
                                           typeSplit, usingArgList)

import           Data.Foldable             (Foldable (toList))
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Tuple.Extra          (uncurry3)
import qualified SyntaxTrees.Haskell.FnDef as S


emptyFnSig :: S.FnSig
emptyFnSig = S.FnSig [] [] [] Nothing

namedFnSig :: H.Type -> [H.Var] -> S.FnSig
namedFnSig tpe args = S.FnSig (typeParam <$> (toList $ findTypeParams tpe))
                      [argList (var <$> args) argTypes]
                      [usingArgList constraints]
                      (Just retType)
 where
   (constraints, rest) = classScopeSplit tpe
   (argTypes, retType) = typeSplit (length args) rest


unNamedFnSig :: H.Type -> Int -> S.FnSig
unNamedFnSig tpe n = S.FnSig (typeParam <$> (toList $ findTypeParams tpe))
                     [argList (S.Var <$> autoFnIds) argTypes]
                     [usingArgList constraints]
                     (Just retType)
 where
   (constraints, rest) = classScopeSplit tpe
   (argTypes, retType) = typeSplit n rest


fnDefs :: (Maybe H.FnSig, Maybe [H.FnDef]) -> S.MethodDef
fnDefs (x, Just [y])
  | (allVars . (.args)) y = simpleFnDef x y
fnDefs (x, y) = fnDef x y

fnDef :: Maybe H.FnSig -> Maybe [H.FnDef] -> S.MethodDef
fnDef sig defs = S.MethodDef [] name fnSig (fnDefToFnBody <$> defs)
  where
    n = maybe 0 (length . (.args)) $ (listToMaybe . (fromMaybe [])) defs
    fnSig = maybe emptyFnSig (`unNamedFnSig` n) $ (.type') <$> sig
    name = var $ maybe ((.name) . head $ fromMaybe [] defs) (.name) sig

simpleFnDef :: Maybe H.FnSig -> H.FnDef -> S.MethodDef
simpleFnDef sig def = S.MethodDef [] name fnSig (Just $ simpleFnDefToFnBody def)
  where
    name = var $ (.name) def
    args = extractVars $ (.args) def
    fnSig = maybe emptyFnSig (`namedFnSig` args) $ (.type') <$> sig


simpleFnDefToFnBody :: H.FnDef -> S.FnBody
simpleFnDefToFnBody def = maybeGuardeBody . (.body) $ def

fnDefToFnBody :: [H.FnDef] -> S.FnBody
fnDefToFnBody defs = match
  where
    match = S.MatchExpr (tuple $ take n autoFnIds) cases
    casePatterns = (S.TuplePattern  . (pattern' <$>) . (.args)) <$> defs
    caseBodies =  (maybeGuardeBody . (.body)) <$> defs
    cases = uncurry3 S.CaseBinding <$> zip3 casePatterns (repeat Nothing)
                                              caseBodies
    tuple x = S.Tuple $ (S.FnVar' . S.Var' . S.Var) <$> x
    n = (length . (.args)) (head defs)


maybeGuardeBody :: H.MaybeGuardedFnBody -> S.FnBody
maybeGuardeBody _ = undefined


fnBody :: H.FnBody -> S.FnBody
fnBody (H.FnApply x y)      = S.FnApply (fnBody x) (fnBody <$> y)
fnBody (H.InfixFnApply x y) = S.InfixFnApply (fnOp x) (fnBody <$> y)
fnBody (H.LambdaExpr x y)   = S.LambdaExpr (var <$> x) (fnBody y)
fnBody (H.LetExpr x y)      = S.LetExpr (Method <$> fnDefOrSigs x) (fnBody y)
fnBody (H.WhereExpr x y)      = S.LetExpr (Method <$> fnDefOrSigs y) (fnBody x)
fnBody (H.IfExpr x y z)      = S.IfExpr (fnBody x) (fnBody y) (fnBody z)
fnBody (H.MultiWayIfExpr Expr x y z) = S.MultiWayIfExpr [] Nothing
fnBody (H.DoExpr x y) =  S.ForExpr (doStep <$> x) (fnBody y)
fnBody (H.CaseOfExpr x y) =  S.MatchExpr (fnBody x) (caseBinding <$> y)
fnBody (H.Tuple x) =  S.Tuple $ fnBody <$> x
fnBody (H.List x) =  S.FnApply (S.FnVar' $ S.Ctor' $ Ctor "List") (fnBody <$> y)
fnBody (H.FnVar' x) = S.FnVar' $ fnVar x
fnBody (H.Literal' x) = S.Literal' $ literal x



doStep (H.DoBinding x y) = S.ForBinding (var x) (fnBody y)
doStep (H.Body x)        = S.ForBinding (S.Var "_") (fnBody x)

caseBinding (H.CaseBinding x y) = S.CaseBinding (pattern' x) Nothing (maybeGuardeBody y)


fnVar (H.Var' x)  = S.Var' $ var x
fnVar (H.Ctor' x) = S.Ctor' $ ctor x

fnOp (H.VarOp' x)  = H.VarOp' $ varOp x
fnOp (H.CtorOp' x) = H.CtorOp' $ ctorOp x


fnDefOrSigs :: [H.FnDefOrSig] -> [(Maybe H.FnSig, Maybe [H.FnDef])]
fnDefOrSigs defsOrSigs = nubBy dedupFn $ mergeUnion sigs groupedDefs

  where
    dedupFn x y = ((.name) <$> fst x) == ((.name) <$> fst y)
    defs = mapMaybe (\case (H.Def x) -> Just ((.name) x, x)
                           (H.Sig _) -> Nothing)
           defsOrSigs
    sigs = mapMaybe (\case (H.Def _) -> Nothing
                           (H.Sig x) -> Just ((.name) x, x))
           defsOrSigs
    groupedDefs = groupTuplesByKey defs

autoFnIds :: [String]
autoFnIds = pure <$> ['x', 'y', 'z', 't', 'u', 'v', 'w', 'p', 'q', 'r', 's']
