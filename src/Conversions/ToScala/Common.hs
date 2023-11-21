module Conversions.ToScala.Common where

import qualified SyntaxTrees.Haskell.Common as H
import qualified SyntaxTrees.Scala.Common   as S

import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Text    (Text)
import qualified Data.Text    as Text
import           Text.Casing  (quietSnake)
import           Utils.String (overText)


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
module' (H.Module x) = S.Package $ Text.map (find moduleCharMap) . replaceNaming . overText quietSnake <$> x

qualifier' :: H.Module -> S.Package
qualifier' (H.Module x) = S.Package $ Text.map (find moduleCharMap) . replaceNaming <$> x


literal :: H.Literal -> S.Literal
literal H.UnitLit       = S.UnitLit
literal (H.BoolLit x)   = S.BoolLit x
literal (H.IntLit x)    = S.IntLit x
literal (H.FloatLit x)  = S.FloatLit x
literal (H.CharLit x)   = S.CharLit x
literal (H.StringLit x) = S.StringLit x



qVar :: H.QVar -> S.QVar
qVar (H.QVar x y) = S.QVar (qualifier' <$> x) (var y)

qVarOp :: H.QVarOp -> S.QVarOp
qVarOp (H.QVarOp x y) = S.QVarOp (qualifier' <$> x) (varOp y)

qCtor :: H.QCtor -> S.QCtor
qCtor (H.QCtor x y) = S.QCtor (qualifier' <$> x) (ctor y)

qCtorOp :: H.QCtorOp -> S.QCtorOp
qCtorOp (H.QCtorOp x y) = S.QCtorOp (qualifier' <$> x) (ctorOp y)

qClass :: H.QClass -> S.QTypeClass
qClass (H.QClass x y) = S.QTypeClass (qualifier' <$> x) (class' y)


replaceNaming :: Text -> Text
replaceNaming = Text.map (find charMap) . find globalMap

find :: Ord k => Map k k -> k -> k
find x y = Map.findWithDefault y y x

globalMap :: Map Text Text
globalMap = varOpMap <> ctorOpMap <> varMap <> ctorMap

moduleCharMap :: Map Char Char
moduleCharMap = Map.fromList [('$', '_')]

charMap :: Map Char Char
charMap = Map.fromList [('\'', '$'), ('.', '|')]

varOpMap :: Map Text Text
varOpMap = Map.fromList [("$", "|<|"), ("#", "|>|"), (".", "<<<"),
                         ("++", "<+>"), ("<>", "<+>"),
                         (",", "Tuple2"), (",,", "Tuple3"),
                         (",,,", "Tuple4"), (",,,,", "Tuple5")]

ctorOpMap :: Map Text Text
ctorOpMap = Map.fromList [(":", "::"), ("::", ":")]

varMap :: Map Text Text
varMap = Map.fromList $ fmap (\x -> (x, x <> "$")) reservedKeyWords

ctorMap :: Map Text Text
ctorMap = Map.fromList [("Just", "Some"), ("Nothing", "None"),
                       ("True", "true"), ("False", "false")]

autoIds :: [Text]
autoIds = Text.singleton <$> ['x', 'y', 'z', 't', 'u', 'v', 'w', 'p', 'q', 'r', 's']

reservedKeyWords :: [Text]
reservedKeyWords = ["type","var","lazy","def","match",
                    "for","else","then" ,"if","match","in",
                    "case","yield","given","class"]
