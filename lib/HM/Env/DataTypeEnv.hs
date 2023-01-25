module HM.Env.DataTypeEnv where

import HM.Type.TypeScheme
import qualified Data.Set as S
import qualified Data.Map as M
import HM.Type.TypeExpr
import HM.Type.Subst
import HM.NameSeed
import HM.Expr

--
-- DATA TYPE ENV
--

{-
  {DataType} = [DataConsType]+
  data T a b c = K b a Int | M c c
  Scheme [a,b,c] (TCons T [a,b,c])

  K -> DataScheme (Scheme [a,b,c] TCons T [a,b,c]) [b,a,Int]
  M -> DataScheme (Scheme [a,b,c] TCons T [a,b,c]) [c,c] 
-}
data DataScheme = DataScheme TypeScheme [TypeExpr]

newtype DataTypeEnv = DataTypeEnv (M.Map ConsName DataScheme)

emptyDataTypeEnv :: DataTypeEnv
emptyDataTypeEnv = DataTypeEnv M.empty

data DataType = DataType TIdentifier [TypeVarName]
data DataConsType = DataConsType ConsName [TypeExpr]

newDataInstance :: NameSeed -> DataScheme -> (Subst, [TypeExpr], TypeExpr)
newDataInstance ns (DataScheme (Scheme snames texpr) texprs) = (sub, texprs', texpr')
  where
    texpr' = applySubstType sub texpr
    texprs' = applySubstTypeList sub texprs
    sub = Subst . M.fromList $ zip (S.toList snames) tnamesSequence
    tnamesSequence = map (TypeVar . TypeVarName) $ varNameSequence ns -- todo: rename

