module Conversions.HaskellToScala.Common where

import qualified SyntaxTrees.Haskell.Common as H
import qualified SyntaxTrees.Scala.Common   as S

import           Data.Char (toLower)
import           Data.Map  (Map)
import qualified Data.Map  as Map



var :: H.Var -> S.Var
var (H.Var x) = S.Var x

ctor :: H.Ctor -> S.Ctor
ctor (H.Ctor x) = S.Ctor x

varOp :: H.VarOp -> S.VarOp
varOp (H.VarOp x) = S.VarOp $ convertOp x

ctorOp :: H.CtorOp -> S.CtorOp
ctorOp (H.CtorOp x) = S.CtorOp $ convertOp x

class' :: H.Class -> S.TypeClass
class' (H.Class x) = S.TypeClass x

module' :: H.Module -> S.Package
module' (H.Module x) = S.Package $ (toLower <$>) <$> init x ++ [last x]


literal :: H.Literal -> S.Literal
literal H.UnitLit       = S.UnitLit
literal (H.BoolLit x)   = S.BoolLit x
literal (H.IntLit x)    = S.IntLit x
literal (H.FloatLit x)  = S.FloatLit x
literal (H.CharLit x)   = S.CharLit x
literal (H.StringLit x) = S.StringLit x


convertOp :: String -> String
convertOp x = filter (/= '`') $ Map.findWithDefault x x operatorMap


operatorMap :: Map String String
operatorMap = Map.fromList [(":", "::"), ("++", "<>")]
