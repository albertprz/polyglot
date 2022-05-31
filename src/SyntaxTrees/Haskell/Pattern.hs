module SyntaxTrees.Haskell.Pattern where

import SyntaxTrees.Haskell.Common (Ctor, Literal, Var, CtorOp)

data Pattern
  = CtorPattern
      { ctor :: Ctor,
        fields :: [Pattern]
      }
  | InfixCtorPattern
      { ctorOp :: CtorOp,
        fields :: [Pattern]
      }
  | RecordPattern
      { ctor :: Ctor,
        namedFields :: [(Var, Maybe Pattern)]
      }
  | WildcardRecordPattern
      { ctor :: Ctor,
        namedFields :: [(Var, Maybe Pattern)]
      }
  | AliasedPattern Var Pattern
  | ListPattern [Pattern]
  | TuplePattern [Pattern]
  | VarPattern Var
  | LitPattern Literal
  | Wildcard
  deriving Show
