module SyntaxTrees.Haskell.Common  where


newtype Var    = Var    String deriving Show
newtype Ctor   = Ctor   String deriving Show
newtype VarOp  = VarOp  String deriving Show
newtype CtorOp = CtorOp String deriving Show
newtype Class  = Class   String  deriving Show
newtype Module = Module [String] deriving Show


data Literal = UnitLit
             | IntLit String
             | FloatLit String
             | CharLit String
             | StringLit String
             deriving Show
