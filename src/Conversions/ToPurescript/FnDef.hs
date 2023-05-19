module Conversions.ToPurescript.FnDef where

import qualified SyntaxTrees.Haskell.FnDef     as H
import qualified SyntaxTrees.Haskell.Type      as H
import qualified SyntaxTrees.Purescript.Common as P
import qualified SyntaxTrees.Purescript.FnDef  as P

import Bookhound.Utils.Foldable         (hasNone)
import Conversions.ToPurescript.Common
import Conversions.ToPurescript.Pattern
import Conversions.ToPurescript.Type
import Data.Foldable                    (Foldable (toList))


fnSig :: H.FnSig -> P.FnSig
fnSig (H.FnSig x y) = P.FnSig (var x) (type' typeScoped)
  where
    typeScoped
      | (H.TypeScope _ _) <- y = y
      | hasNone $ findTypeParams y = y
      | otherwise             = H.TypeScope (toList $ findTypeParams y) y

fnDef :: H.FnDef -> P.FnDef
fnDef (H.FnDef x y z) =
  P.FnDef (var <$> x) (pattern' <$> y) (maybeGuardedFnBody z)


-- TODO: Infix annotations for infix functions
infixFnAnnotation :: H.InfixFnAnnotation -> P.InfixFnDef
infixFnAnnotation (H.InfixFnAnnotation x y z) =
  P.InfixFnDef (associativity x) y (P.Var "var") (varOp z)

fnDefOrSig :: H.FnDefOrSig -> P.FnDefOrSig
fnDefOrSig (H.Def x) = P.Def $ fnDef x
fnDefOrSig (H.Sig x) = P.Sig $ fnSig x

fnBody :: H.FnBody -> P.FnBody
fnBody (H.FnApply x y)      = P.FnApply (fnBody x) (fnBody <$> y)
fnBody (H.InfixFnApply x y) = P.InfixFnApply (fnOp <$> x) (fnBody <$> y)

fnBody (H.LeftOpSection x y)    = P.LeftOpSection (fnOp x) (fnBody y)
fnBody (H.RightOpSection x y)   = P.RightOpSection (fnBody x) (fnOp y)
fnBody (H.PostFixOpSection x y) = P.PostFixOpSection (fnBody x) (fnOp y)

fnBody (H.LambdaExpr x y)   = P.LambdaExpr (pattern' <$> x) (fnBody y)
fnBody (H.LetExpr x y)      = P.LetExpr (fnDefOrSig <$> x) (fnBody y)
fnBody (H.WhereExpr x y)    = P.WhereExpr (fnBody x) (fnDefOrSig <$> y)

fnBody (H.IfExpr x y z)     = P.IfExpr (fnBody x) (fnBody y) (fnBody z)
fnBody (H.MultiWayIfExpr x) = P.MultiWayIfExpr $ guardedFnBody <$> x
fnBody (H.DoExpr x)         = P.DoExpr $ doStep <$> x
fnBody (H.CaseOfExpr x y)   = P.CaseOfExpr (fnBody x) (caseBinding <$> y)
fnBody (H.LambdaCaseExpr x) = P.LambdaCaseExpr $ caseBinding <$> x
fnBody (H.RecordCreate x y) = P.RecordCreate (qCtor x)
                                ((\(z, t) -> (var z, fnBody t)) <$> y)
fnBody (H.RecordUpdate x y) = P.RecordUpdate (qVar x)
                                ((\(z, t) -> (var z, fnBody t)) <$> y)
fnBody (H.Tuple x)          = P.Tuple $ fnBody <$> x
fnBody (H.List x)           = P.Array $ fnBody <$> x
fnBody (H.FnVar' x)         = P.FnVar' $ fnVar x
fnBody (H.Literal' x)       = P.Literal' $ literal x


fnVar :: H.FnVar -> P.FnVar
fnVar (H.Selector x)    = P.Selector $ var x
fnVar (H.Selection x y) = P.Selection (qVar x) (var <$> y)
fnVar (H.Var' x)        = P.Var' $ qVar x
fnVar (H.Ctor' x)       = P.Ctor' $ qCtor x

fnOp :: H.FnOp -> P.FnOp
fnOp (H.VarOp' x)  = P.VarOp' $ qVarOp x
fnOp (H.CtorOp' x) = P.CtorOp' $ qCtorOp x


doStep :: H.DoStep -> P.DoStep
doStep (H.DoBinding x y) = P.DoBinding (var <$> x) (fnBody y)
doStep (H.LetBinding x)  = P.LetBinding (fnDefOrSig <$> x)
doStep (H.Body x)        = P.Body $ fnBody x


caseBinding :: H.CaseBinding -> P.CaseBinding
caseBinding (H.CaseBinding x y) =
  P.CaseBinding (pattern' x) (maybeGuardedFnBody y)


maybeGuardedFnBody :: H.MaybeGuardedFnBody -> P.MaybeGuardedFnBody
maybeGuardedFnBody (H.Guarded x)  = P.Guarded $ guardedFnBody <$> x
maybeGuardedFnBody (H.Standard x) = P.Standard $ fnBody x


guardedFnBody :: H.GuardedFnBody -> P.GuardedFnBody
guardedFnBody (H.GuardedFnBody x y) =
  P.GuardedFnBody (guard x) (fnBody y)

guard :: H.Guard -> P.Guard
guard (H.Guard x) = P.Guard $ patternGuard <$> x
guard H.Otherwise = P.Otherwise

patternGuard :: H.PatternGuard -> P.PatternGuard
patternGuard (H.PatternGuard x y) =
  P.PatternGuard (pattern' x) (fnBody y)
patternGuard (H.SimpleGuard x) =
  P.SimpleGuard (fnBody x)

associativity :: H.Associativity -> P.Associativity
associativity H.LAssoc = P.LAssoc
associativity H.RAssoc = P.RAssoc
