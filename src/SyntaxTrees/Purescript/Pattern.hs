module SyntaxTrees.Purescript.Pattern where

import SyntaxTrees.Purescript.Common (Literal, QCtor, QCtorOp, Var)


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
  | ListPattern [Pattern]
  | TuplePattern [Pattern]
  | VarPattern Var
  | LitPattern Literal
  | Wildcard
  deriving (Show)
