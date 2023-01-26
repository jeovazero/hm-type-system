module HM.Env where

import qualified Data.Set as S
import qualified Data.Map as M
import HM.Type.TypeExpr
import HM.Type.Subst
import HM.Type.Ops
import HM.Type.TypeScheme
import HM.Env.TypeEnv
import HM.Env.DataTypeEnv
import HM.NameSeed
import HM.Expr
import Data.List

--
-- ENV
--

data Env = Env TypeEnv DataTypeEnv NameSeed

startEnv :: Env
startEnv = Env emptyTypeEnv emptyDataTypeEnv startNameSeed

nameSeed (Env _ _ ns) = ns
typeEnv (Env t _ _) = t
dataTypeEnv (Env _ d _) = d

applySubstTypeEnv :: Subst -> Env -> Env
applySubstTypeEnv sub (Env (TypeEnv tenv) d n) =
  Env (TypeEnv $ M.map (applySubstScheme sub) tenv) d n

increaseNameSeed :: Env -> Env
increaseNameSeed (Env t d n) = Env t d (increaseNsPrefix n)
updateNameSeed n' (Env t d _) = Env t d n'

nextTypeVarName :: Env -> TypeVarName
nextTypeVarName = TypeVarName . nextNameSeed

nextNameSeed (Env _ _ n) = varNameNs n

updateTypeEnv :: TypeEnv -> Env -> Env
updateTypeEnv t' (Env _ d n) = Env t' d n

lookupTypeEnv :: VarName -> Env -> Maybe TypeScheme
lookupTypeEnv var (Env (TypeEnv t) _ _) = M.lookup var t

insertVarTypeEnv :: (VarName, TypeExpr) -> Env -> Env
insertVarTypeEnv pair (Env t d n) = Env (insertVarTypeEnv' pair t) d n

lookupDataEnv :: ConsName -> Env -> Maybe DataScheme
lookupDataEnv cname (Env _ (DataTypeEnv d) _) = M.lookup cname d

unionTypeEnv :: Env -> TypeEnv -> Env
unionTypeEnv (Env (TypeEnv t1) d n) (TypeEnv t2) = Env (TypeEnv $ M.union t2 t1) d n

isInTypeEnvScope :: VarName -> Env -> Bool
isInTypeEnvScope varn (Env (TypeEnv tenv) _ _) = M.member varn tenv

addDeclarations :: Env -> [VarName] -> [TypeExpr] -> Env
addDeclarations (Env te@(TypeEnv tenv) d ns) vars texprs =
  Env (TypeEnv $ foldl' (\t (x, s) -> M.insert x s t) tenv (zip vars schemes)) d ns
 where
  schemes = map (generalization freeVars ns) texprs
  freeVars = S.fromList $ freeVarsTypeEnv te

insertDataTypeEnv :: DataType -> [DataConsType] -> Env -> Env
insertDataTypeEnv (DataType tid tvnames) dataCons (Env t (DataTypeEnv denv) n) =
  let tscheme = Scheme (S.fromList tvnames) (TypeCons tid $ fmap TypeVar tvnames)
      consPairs = fmap (\(DataConsType cname texprs) -> (cname,texprs)) dataCons
      insertCons acc (name,texprs) = M.insert name (DataScheme tscheme texprs) acc
      denv' = foldl' insertCons denv consPairs
    in Env t (DataTypeEnv denv') n

