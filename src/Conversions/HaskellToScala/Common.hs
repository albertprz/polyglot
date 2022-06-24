module Conversions.HaskellToScala.Common where

import qualified SyntaxTrees.Haskell.Common as H
import qualified SyntaxTrees.Scala.Common   as S

import           Data.Char (toLower)
import           Data.Map  (Map)
import qualified Data.Map  as Map



var :: H.Var -> S.Var
var (H.Var x) = S.Var $ replaceNaming x

ctor :: H.Ctor -> S.Ctor
ctor (H.Ctor x) = S.Ctor $ replaceNaming x

varOp :: H.VarOp -> S.VarOp
varOp (H.VarOp x) = S.VarOp $ replaceNaming x

ctorOp :: H.CtorOp -> S.CtorOp
ctorOp (H.CtorOp x) = S.CtorOp $ replaceNaming x

class' :: H.Class -> S.TypeClass
class' (H.Class x) = S.TypeClass x


module' :: H.Module -> S.Package
module' (H.Module x) = S.Package $ ((toLower <$>) . replaceNaming) <$> x


literal :: H.Literal -> S.Literal
literal H.UnitLit       = S.UnitLit
literal (H.BoolLit x)   = S.BoolLit x
literal (H.IntLit x)    = S.IntLit x
literal (H.FloatLit x)  = S.FloatLit x
literal (H.CharLit x)   = S.CharLit x
literal (H.StringLit x) = S.StringLit x



qVar :: H.QVar -> S.QVar
qVar (H.QVar x y) = S.QVar (module' <$> x) (var y)

qVarOp :: H.QVarOp -> S.QVarOp
qVarOp (H.QVarOp x y) = S.QVarOp (module' <$> x) (varOp y)

qCtor :: H.QCtor -> S.QCtor
qCtor (H.QCtor x y) = S.QCtor (module' <$> x) (ctor y)

qCtorOp :: H.QCtorOp -> S.QCtorOp
qCtorOp (H.QCtorOp x y) = S.QCtorOp (module' <$> x) (ctorOp y)

qClass :: H.QClass -> S.QTypeClass
qClass (H.QClass x y) = S.QTypeClass (module' <$> x) (class' y)


replaceNaming :: String -> String
replaceNaming x = find charMap <$> find globalMap x

find :: Ord k => Map k k -> k -> k
find x y = Map.findWithDefault y y x

globalMap :: Map String String
globalMap = varOpMap <> ctorOpMap <> varMap <> ctorMap <> moduleMap


charMap :: Map Char Char
charMap = Map.fromList [('\'', '_'), ('$', '%'), ('.', '|')]

varOpMap :: Map String String
varOpMap = Map.fromList [("$", "<|:"), (".", "|:"), ("++", "<+>"), ("<>", "<+>")]

ctorOpMap :: Map String String
ctorOpMap = Map.fromList [(":", "::"), ("::", ":")]

varMap :: Map String String
varMap = Map.fromList [("type", "typex"), ("var", "varx")]

moduleMap :: Map String String
moduleMap = Map.fromList [("Type", "Typex"), ("Var", "Varx")]

ctorMap :: Map String String
ctorMap = Map.fromList [("Just", "Some"), ("Nothing", "None")]
