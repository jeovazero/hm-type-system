module HM.Env.TypeEnv where

import HM.Type.TypeScheme
import qualified Data.Set as S
import qualified Data.Map as M
import HM.Type.TypeExpr
import HM.Type.Subst
import HM.NameSeed
import HM.Expr
import Data.Maybe (mapMaybe)

--
-- TYPE ENV
--

newtype TypeEnv = TypeEnv (M.Map VarName TypeScheme) deriving (Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv M.empty

freeVarsTypeEnv :: TypeEnv -> [TypeVarName]
freeVarsTypeEnv (TypeEnv tenv) = concatMap freeVarsScheme (M.elems tenv)

applySubstTypeEnv' :: Subst -> TypeEnv -> TypeEnv
applySubstTypeEnv' sub (TypeEnv tenv) = TypeEnv $ M.map (applySubstScheme sub) tenv

insertVarTypeEnv' :: (VarName, TypeExpr) -> TypeEnv -> TypeEnv
insertVarTypeEnv' (v,texpr) (TypeEnv tenv) =
  TypeEnv $ M.insert v (Scheme S.empty texpr) tenv

typeEnvFromVars :: NameSeed -> [VarName] -> TypeEnv
typeEnvFromVars ns vars =
  let schemeVars = zipVarsScheme vars ns
   in TypeEnv $ M.fromList schemeVars

unionTypeEnv' :: TypeEnv -> TypeEnv -> TypeEnv
unionTypeEnv' (TypeEnv te1) (TypeEnv te2) = TypeEnv $ M.union te2 te1

typesFromVarsTenv :: [VarName] -> TypeEnv -> [TypeExpr]
typesFromVarsTenv vars (TypeEnv tenv) =
  mapMaybe (\v -> fmap typeExprFromScheme $ M.lookup v tenv ) vars

keysTenv :: TypeEnv -> [VarName]
keysTenv (TypeEnv tenv) = M.keys tenv

