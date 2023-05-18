module SyntaxTrees.Scala.Pattern where

import SyntaxTrees.Scala.Common (Literal, QCtor, QCtorOp, Var)
import Utils.String

data Pattern
  = CtorPattern
      { ctor   :: QCtor
      , fields :: [Pattern]
      }
  | InfixCtorPattern
      { ctorOp :: QCtorOp
      , fields :: [Pattern]
      }
  | AliasedPattern Var Pattern
  | TuplePattern [Pattern]
  | VarPattern Var
  | LitPattern Literal
  | Wildcard



instance Show Pattern where
  show (CtorPattern x y)      = show x +++ wrapParensCsv y
  show (InfixCtorPattern x y) = str (wrapSpaces $ show x) y
  show (AliasedPattern x y)   = wrapParens $ show x +++ "@" +++ show y
  show (TuplePattern [x])     = show x
  show (TuplePattern x)       = wrapParensCsv x
  show (VarPattern x)         = show x
  show (LitPattern x)         = show x
  show Wildcard               = "_"
