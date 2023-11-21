module Conversions.ToPurescript.Common where

import qualified SyntaxTrees.Haskell.Common    as H
import qualified SyntaxTrees.Purescript.Common as P

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text)
import qualified Data.Text as Text


var :: H.Var -> P.Var
var (H.Var x) = P.Var $ replaceNaming x

ctor :: H.Ctor -> P.Ctor
ctor (H.Ctor x) = P.Ctor $ replaceNaming x

varOp :: H.VarOp -> P.VarOp
varOp (H.VarOp x) = P.VarOp $ replaceNaming x

ctorOp :: H.CtorOp -> P.CtorOp
ctorOp (H.CtorOp x) = P.CtorOp $ replaceNaming x

varOpFn :: H.Var -> P.Var
varOpFn (H.Var x) = P.Var $ find varOpFnMap $ replaceNaming x

class' :: H.Class -> P.Class
class' (H.Class x) = P.Class x


module' :: H.Module -> P.Module
module' (H.Module x) = P.Module $ replaceNaming <$> x

qualifier' :: H.Module -> P.Module
qualifier' (H.Module x) = P.Module $ replaceNaming <$> x


literal :: H.Literal -> P.Literal
literal H.UnitLit       = P.UnitLit
literal (H.BoolLit x)   = P.BoolLit x
literal (H.IntLit x)    = P.IntLit x
literal (H.FloatLit x)  = P.NumberLit x
literal (H.CharLit x)   = P.CharLit x
literal (H.StringLit x) = P.StringLit x



qVar :: H.QVar -> P.QVar
qVar (H.QVar x y) = P.QVar (qualifier' <$> x) (var y)

qVarOp :: H.QVarOp -> P.QVarOp
qVarOp (H.QVarOp x y) = P.QVarOp (qualifier' <$> x) (varOp y)

qCtor :: H.QCtor -> P.QCtor
qCtor (H.QCtor x y) = P.QCtor (qualifier' <$> x) (ctor y)

qCtorOp :: H.QCtorOp -> P.QCtorOp
qCtorOp (H.QCtorOp x y) = P.QCtorOp (qualifier' <$> x) (ctorOp y)

qClass :: H.QClass -> P.QClass
qClass (H.QClass x y) = P.QClass (qualifier' <$> x) (class' y)


replaceNaming :: Text -> Text
replaceNaming = Text.map (find charMap) . find globalMap

find :: Ord k => Map k k -> k -> k
find x y = Map.findWithDefault y y x

globalMap :: Map Text Text
globalMap = varOpMap <> ctorOpMap <> varMap <> ctorMap


charMap :: Map Char Char
charMap = Map.empty

varOpMap :: Map Text Text
varOpMap = Map.fromList [(".", "<<<"), ("++", "<>"),
                         ("<&>", "<#>"),
                         (",", "/\\"), (",,", "Tuple3"),
                         (",,,", "Tuple4"), (",,,,", "Tuple5")]

ctorOpMap :: Map Text Text
ctorOpMap = Map.empty

varMap :: Map Text Text
varMap = Map.fromList [("fmap", "map"), ("id", "identity"),
                       ("toList", "toUnfoldable"),
                       ("maybeToList",  "fromFoldable"),
                       ("first", "lmap"), ("second", "rmap") ]

ctorMap :: Map Text Text
ctorMap = Map.fromList [("True", "true"), ("False", "false")]

varOpFnMap :: Map Text Text
varOpFnMap = Map.fromList [("==", "eq"), ("/=", "notEq"),
                         ("<>", "append"), ("<$>", "map"),
                         ("<*>", "apply"), ("<|>", "alt"),
                         (">>=", "bind") ]
