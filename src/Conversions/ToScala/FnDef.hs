module Conversions.ToScala.FnDef where

import qualified SyntaxTrees.Haskell.Common as H
import qualified SyntaxTrees.Haskell.FnDef  as H
import qualified SyntaxTrees.Haskell.Type   as H
import qualified SyntaxTrees.Scala.Common   as S
import qualified SyntaxTrees.Scala.FnDef    as S
import qualified SyntaxTrees.Scala.Pattern  as S

import Conversions.ToScala.Common  (autoIds, literal, qCtor, qCtorOp, qVar,
                                    qVarOp, var)
import Conversions.ToScala.Pattern (allVars, extractVars, pattern')
import Conversions.ToScala.Type    (argLists, classScopeSplit, findTypeParams,
                                    typeParam, typeSplit, usingArgList)

import Data.Foldable           (Foldable (fold, toList))
import Data.List               (nubBy)
import Data.Maybe              (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Tuple.Extra        (uncurry3, (***))
import SyntaxTrees.Scala.FnDef (WhenExpr (..))
import Utils.List              (groupTuplesByKey, mergeUnion)


emptyFnSig :: S.FnSig
emptyFnSig = S.FnSig [] [] [] Nothing

namedFnSig :: H.Type -> [H.Var] -> S.FnSig
namedFnSig tpe args = S.FnSig (typeParam <$> (toList $ findTypeParams tpe))
                      (argLists (var <$> args) argTypes)
                      [usingArgList constraints]
                      (Just retType)
  where
    (constraints, rest) = classScopeSplit tpe
    (argTypes, retType) = typeSplit (length args) rest


unNamedFnSig :: H.Type -> Int -> S.FnSig
unNamedFnSig tpe n = S.FnSig (typeParam <$> (toList $ findTypeParams tpe))
                     (argLists (S.Var <$> autoIds) argTypes)
                     [usingArgList constraints]
                     (Just retType)
  where
    (constraints, rest) = classScopeSplit tpe
    (argTypes, retType) = typeSplit n rest


fnDefOrSigs :: [H.FnDefOrSig] -> [(Maybe H.FnSig, Maybe [H.FnDef])]
fnDefOrSigs defsOrSigs = nubBy dedupFn $ mergeUnion sigs groupedDefs

  where
    dedupFn x y = any isJust [fst x, fst y] &&
                  ((.name) <$> fst x) == ((.name) <$> fst y)
    defs = mapMaybe (\case (H.Def x) -> Just (head x.names, x)
                           (H.Sig _) -> Nothing)
           defsOrSigs
    sigs = mapMaybe (\case (H.Def _) -> Nothing
                           (H.Sig x) -> Just (x.name, x))
           defsOrSigs
    groupedDefs = groupTuplesByKey defs



fnDefs :: (Maybe H.FnSig, Maybe [H.FnDef]) -> S.InternalFnDef
fnDefs (x, Just [y])
  | length  y.names > 1 = valDef y
  | allVars y.args      = simpleFnDef x y
fnDefs (x, y)           = fnDef x y


fnDef :: Maybe H.FnSig -> Maybe [H.FnDef] -> S.InternalFnDef
fnDef sig defs = S.FnMethod $ S.MethodDef [] name fnSig
                              (matchFn . fnDefToFnBody <$> defs)
  where
    n = maybe 0 (length . (.args)) $ (listToMaybe . fold) defs
    fnSig =  (`unNamedFnSig` n') . (.type') <$> sig
    name = var $ maybe (head . (.names) . head $ fromMaybe [] defs) (.name) sig
    (n', matchFn) = if n == 1 then (0, topLevelMatch) else (n, id)

simpleFnDef :: Maybe H.FnSig -> H.FnDef -> S.InternalFnDef
simpleFnDef sig def = S.FnMethod $ S.MethodDef [] name fnSig
                                               (Just $ simpleFnDefToFnBody def)
  where
    name = var $ head def.names
    args = extractVars $ (.args) def
    fnSig = (`namedFnSig` args) . (.type') <$> sig

valDef :: H.FnDef -> S.InternalFnDef
valDef def = S.FnVal $ S.ValDef [S.Lazy] names Nothing
                                (Just $ simpleFnDefToFnBody def)
  where
    names = var <$> def.names


simpleFnDefToFnBody :: H.FnDef -> S.FnBody
simpleFnDefToFnBody def = maybeGuardedBody . (.body) $ def

fnDefToFnBody :: [H.FnDef] -> S.FnBody
fnDefToFnBody defs = match
  where
    match = simplifyMatch $
      S.MatchExpr (tuple $ take n autoIds) cases
    casePatterns = (S.TuplePattern  . (pattern' <$>) . (.args)) <$> defs
    caseBodies =  (maybeGuardedBody . (.body)) <$> defs
    cases = uncurry3 S.CaseBinding <$> zip3 casePatterns (repeat Nothing)
                                              caseBodies
    tuple x = S.Tuple $ (S.FnVar' . S.Var' . S.QVar Nothing . S.Var) <$> x
    n = (length . (.args)) (head defs)



fnBody :: H.FnBody -> S.FnBody
fnBody (H.FnApply x y)      = S.FnApply (fnBody x) (fnBody <$> y)
fnBody (H.InfixFnApply x y) = S.InfixFnApply (fnOp <$> x) (fnBody <$> y)

fnBody (H.LeftOpSection x y) = S.LambdaExpr [] (S.InfixFnApply [fnOp x]
        [S.FnVar' $ S.Var' $ S.QVar Nothing $ S.Var "_", fnBody y])

fnBody (H.RightOpSection x y) = S.LambdaExpr [] (S.InfixFnApply [fnOp y]
        [fnBody x, S.FnVar' $ S.Var' $ S.QVar Nothing $ S.Var "_"])

fnBody (H.PostFixOpSection x y) = S.LambdaExpr [] (S.InfixFnApply [fnOp y]
                                                   [fnBody x])
fnBody (H.LambdaExpr x y)   = S.LambdaExpr (pattern' <$> x)
                                          (fnBody y)
fnBody (H.IfExpr x y z)     = S.IfExpr (fnBody x) (fnBody y) (fnBody z)
fnBody (H.DoExpr x)         = S.ForExpr (doStep <$> init x) (extractDoStep $ last x)
fnBody (H.MultiWayIfExpr x) = maybeGuardedBody $ H.Guarded x
fnBody (H.CaseOfExpr x y)   = simplifyMatch $
                              S.MatchExpr (fnBody x) (caseBinding <$> y)
fnBody (H.LambdaCaseExpr x) = S.LambdaExpr [] (fnBody $ H.CaseOfExpr
                              (H.FnVar' $ H.Var' $ H.QVar Nothing $ H.Var "_") x)
fnBody (H.Tuple x)          = S.Tuple $ fnBody <$> x
fnBody (H.FnVar' x)         = fnVar x
fnBody (H.Literal' x)       = S.Literal' $ literal x

fnBody (H.List [])        = S.FnApply
        (S.FnVar' $ S.Ctor' $ S.QCtor (Just $ S.Package ["List"])
                                      (S.Ctor "empty")) []
fnBody (H.List x)        = S.FnApply
        (S.FnVar' $ S.Ctor' $ S.QCtor Nothing $ S.Ctor "List")
                                     (fnBody <$> x)
fnBody (H.LetExpr x y)   = S.LetExpr (fnDefs <$> fnDefOrSigs x)
                                     (fnBody y)
fnBody (H.WhereExpr x y) = S.LetExpr (fnDefs <$> fnDefOrSigs y)
                                     (fnBody x)
fnBody (H.RecordCreate x y) = S.NamedFnApply (S.FnVar' $ S.Ctor' $ qCtor x)
                                             ((var *** fnBody) <$> y)
fnBody (H.RecordUpdate x y) = S.NamedFnApply (S.FnVar' $ S.Selection (qVar x)
                                              [S.Var "copy"])
                                             ((var *** fnBody) <$> y)




doStep :: H.DoStep -> S.ForStep
doStep (H.DoBinding x y) = S.ForBinding (var <$> x) (fnBody y)
doStep (H.LetBinding x)  = S.LetBinding (fnDefs <$> fnDefOrSigs x)
doStep (H.Body x)        = S.ForBinding [S.Var "_"] (fnBody x)



caseBinding :: H.CaseBinding -> S.CaseBinding
caseBinding (H.CaseBinding x y) = S.CaseBinding (pattern' x) Nothing
                                                (maybeGuardedBody y)


simplifyMatch :: S.FnBody -> S.FnBody
simplifyMatch (S.MatchExpr x y) = S.MatchExpr x (foldMap simplifyCases y)
simplifyMatch x                 = x

topLevelMatch :: S.FnBody -> S.FnBody
topLevelMatch (S.MatchExpr _ y) = S.TopLevelMatchExpr y
topLevelMatch x                 = x


simplifyCases :: S.CaseBinding -> [S.CaseBinding]
simplifyCases (S.CaseBinding x' Nothing (S.MatchExpr _ y))
  | all (isJust . simpleCase) y = uncurry (S.CaseBinding x') <$> z
    where
      z = mapMaybe simpleCase y
simplifyCases x = [x]


simpleCase :: S.CaseBinding -> Maybe (Maybe S.FnBody, S.FnBody)
simpleCase (S.CaseBinding S.Wildcard x y) = Just (x, y)
simpleCase _                              = Nothing


maybeGuardedBody :: H.MaybeGuardedFnBody -> S.FnBody
maybeGuardedBody (H.Guarded x)
  | H.Otherwise <-  last guards
  , all onlySimpleGuards $ init guards
  = S.CondExpr whenBranches elseBranch
  where
    guards = (.guard) <$> x
    bodies = fnBody . (.body)  <$> x
    conds = aggregateConds . (fnBody <$>) . extractSimpleGuards <$> (init guards)
    whenBranches = uncurry WhenExpr <$> zip conds (init bodies)
    elseBranch = last bodies

maybeGuardedBody (H.Guarded x)  = simplifyMatch $
  S.MatchExpr (S.Literal' $ S.BoolLit True) (guardedBody <$> x)
maybeGuardedBody (H.Standard x) = fnBody x


guardedBody :: H.GuardedFnBody -> S.CaseBinding
guardedBody (H.GuardedFnBody x@(H.Guard y) body)
      | onlySimpleGuards x =
        S.CaseBinding S.Wildcard (Just $ aggregateConds $
                                         fnBody <$> extractSimpleGuards x)
                                 (fnBody body)

      | otherwise =
        S.CaseBinding S.Wildcard Nothing $ foldr patternGuard (fnBody body) y

guardedBody (H.GuardedFnBody H.Otherwise body) =
  S.CaseBinding S.Wildcard Nothing $ fnBody body


patternGuard :: H.PatternGuard -> S.FnBody -> S.FnBody
patternGuard (H.PatternGuard x y) body = simplifyMatch $
  S.MatchExpr
        (fnBody y)
        [S.CaseBinding (pattern' x) Nothing body]

patternGuard (H.SimpleGuard x) body = simplifyMatch $
  S.MatchExpr
        (S.Literal' $ S.BoolLit True)
        [S.CaseBinding S.Wildcard (Just (fnBody x)) body]


fnVar :: H.FnVar -> S.FnBody
fnVar (H.Selector x)    = S.LambdaExpr [] $ S.FnVar' $
                                S.Selection (S.QVar Nothing $ S.Var "_") [var x]
fnVar (H.Selection x y) = S.FnVar' $ S.Selection (qVar x) (var <$> y)
fnVar (H.Var' x)        = S.FnVar' $ S.Var' $ qVar x
fnVar (H.Ctor' x)       = S.FnVar' $ S.Ctor' $ qCtor x


fnOp :: H.FnOp -> S.FnOp
fnOp (H.VarOp' x)  = S.VarOp' $ qVarOp x
fnOp (H.CtorOp' x) = S.CtorOp' $ qCtorOp x


extractDoStep :: H.DoStep -> S.FnBody
extractDoStep (H.DoBinding _ y) = fnBody y
extractDoStep (H.Body x)        = fnBody x
extractDoStep (H.LetBinding  _) = S.Tuple []


extractSimpleGuards :: H.Guard -> [H.FnBody]
extractSimpleGuards (H.Guard x) =
  foldMap (\case (H.SimpleGuard y) -> [y]; _ -> []) x
extractSimpleGuards _ = []


onlySimpleGuards :: H.Guard -> Bool
onlySimpleGuards (H.Guard x) =
  all (\case (H.SimpleGuard _) -> True; _ -> False ) x
onlySimpleGuards H.Otherwise = False


aggregateConds :: [S.FnBody] -> S.FnBody
aggregateConds [x] = x
aggregateConds x = S.InfixFnApply
                     [S.VarOp' $ S.QVarOp Nothing $ S.VarOp "&&"] x
