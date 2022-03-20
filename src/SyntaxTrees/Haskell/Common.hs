module SyntaxTrees.Haskell.Common  where

data Literal = IntLit String |
               FloatLit String |
               CharLit String |
               StringLit String deriving Show

newtype Var    = Var String deriving Show
newtype Ctor   = Ctor String deriving Show
newtype VarOp  = VarOp String deriving Show
newtype CtorOp = CtorOp String deriving Show

newtype TypeVar   = TypeVar String deriving Show
newtype TypeCtor  = TypeCtor String deriving Show
newtype TypeParam = TypeParam String deriving Show

data AnyKindedType = ZeroKinded Type |
                     NonZeroKinded TypeCtor deriving Show

newtype Class  = Class String deriving Show
newtype Module = Module String deriving Show

data Type = Arrow      [Type] |
            TypeApply  TypeCtor [Type] |
            TypeVar'   TypeVar |
            TypeParam' TypeParam deriving Show

data Pattern = CtorPattern {
    ctor   :: Ctor
  , fields :: [Pattern]
} | VarPattern Var
  | LitPattern Literal
  | Wildcard  deriving Show
