module SyntaxTrees.Purescript.Pattern where

import Data.List                     (intercalate)
import SyntaxTrees.Purescript.Common (Literal, QCtor, QCtorOp, Var)
import Utils.String                  (Wrapper (Wrapper), joinMaybe,
                                      wrapCurlyCsv, wrapParens, wrapParensCsv,
                                      wrapSpaces, wrapSquareCsv, (+++))


data Pattern
  = CtorPattern
      { ctor   :: QCtor
      , fields :: [Pattern]
      }
  | InfixCtorPattern
      { ctorOp :: QCtorOp
      , fields :: [Pattern]
      }
  | RecordPattern
      { ctor        :: QCtor
      , namedFields :: [(Var, Maybe Pattern)]
      }
  | AliasedPattern Var Pattern
  | ArrayPattern [Pattern]
  | TuplePattern [Pattern]
  | VarPattern Var
  | LitPattern Literal
  | Wildcard



instance Show Pattern where
  show (CtorPattern x y)      = show x +++ intercalate " " (showPatternNested <$> y)
  show (InfixCtorPattern x y) = intercalate (wrapSpaces $ show x) (showInfixPatternNested <$> y)
  show (RecordPattern x y)    = show x +++
    (wrapCurlyCsv $ Wrapper . showRecordFieldPattern <$> y)
  show (AliasedPattern x y)   = show x +++ "@" +++ showPatternNested y
  show (ArrayPattern x)       = wrapSquareCsv x
  show (TuplePattern [x])     = show x
  show (TuplePattern x)       = wrapParensCsv x
  show (VarPattern x)         = show x
  show (LitPattern x)         = show x
  show Wildcard               = "_"


showRecordFieldPattern :: (Var, Maybe Pattern) -> String
showRecordFieldPattern (x, y) = show x ++ joinMaybe " =" y

showInfixPatternNested :: Pattern -> String
showInfixPatternNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      (InfixCtorPattern _ _) -> True
      _                      -> False

showPatternNested :: Pattern -> String
showPatternNested x = transformFn $ show x
  where
    transformFn = if shouldWrap then wrapParens else id
    shouldWrap = case x of
      (CtorPattern _ _)      -> True
      (InfixCtorPattern _ _) -> True
      (AliasedPattern _ _)   -> True
      _                      -> False
