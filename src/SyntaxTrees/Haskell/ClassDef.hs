module SyntaxTrees.Haskell.ClassDef  where

import SyntaxTrees.Haskell.Term ( TermName )
import SyntaxTrees.Haskell.FnDef ( LocalFnDef, FnSig )


data ClassDef = ClassDef {
    name :: TermName
  , typeParam :: TermName
  , sigs :: [FnSig]
}

data InstanceDef = InstanceDef {
    className :: TermName
  , typeName  :: TermName
  , defs      :: LocalFnDef
}
