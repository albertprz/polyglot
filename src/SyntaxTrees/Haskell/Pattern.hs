module SyntaxTrees.Haskell.Pattern where

import SyntaxTrees.Haskell.Common (Literal, QCtor, QCtorOp, Var)
import SyntaxTrees.Haskell.Type   (Type)


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
  | WildcardRecordPattern
      { ctor        :: QCtor
      , namedFields :: [(Var, Maybe Pattern)]
      }
  | AliasedPattern Var Pattern
  | TypeAnnotation Pattern Type
  | ListPattern [Pattern]
  | TuplePattern [Pattern]
  | VarPattern Var
  | LitPattern Literal
  | Wildcard
