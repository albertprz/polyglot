module SyntaxTrees.Haskell.DataDef  where

import SyntaxTrees.Haskell.Term ( TermName )

data TypeDef = TypeDef {
    typeAlias  :: TermName
  , actualType :: TermName
}

data NewTypeDef = NewTypeDef TermName FieldDef

data DataDef = DataDef TermName [DataCtorDef]

data DataCtorDef = UnNamedFieldsCtor [UnNamedFieldDef] |
                   NamedFieldsCtor   [NamedFieldDef]

newtype UnNamedFieldDef = UnNamedFieldDef TermName
data    NamedFieldDef   = NamedFieldDef {
     fieldName :: TermName
  ,  fieldType :: TermName
}

data FieldDef = UnNamedField UnNamedFieldDef |
                NamedField   NamedFieldDef
