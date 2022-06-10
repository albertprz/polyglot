module SyntaxTrees.Scala.Pattern where

import SyntaxTrees.Scala.Common (Ctor, CtorOp, Literal, Var)

data Pattern
  = CtorPattern
      { ctor   :: Ctor
      , fields :: [Pattern]
      }
  | InfixCtorPattern
      { ctorOp :: CtorOp
      , fields :: [Pattern]
      }
  | RecordPattern
      { ctor        :: Ctor
      , namedFields :: [(Var, Maybe Pattern)]
      }
  | WildcardRecordPattern
      { ctor        :: Ctor
      , namedFields :: [(Var, Maybe Pattern)]
      }
  | AliasedPattern Var Pattern
  | TuplePattern [Pattern]
  | VarPattern Var
  | LitPattern Literal
  | Wildcard
  deriving (Show)
