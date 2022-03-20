module SyntaxTrees.Haskell.Common  where

type Literal = String
type Term    = String

type SimpleType    = String
type TypeParameter = String
type TypeCtor      = String

type Class  = String
type Module = String

data Type = SimpleType' SimpleType |
            TypeParameter' TypeParameter |
            TypeCtor' TypeCtor [Type] |
            Arrow' [Type]

data Pattern = CtorPattern' CtorPattern |
               VarPattern' Term |
               LitPattern' Literal

data CtorPattern = CtorPattern {
    ctor   :: Term
  , fields :: [Term]
}
