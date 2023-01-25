module HM.Type.Subst where

import qualified Data.Map as M
import HM.Type.TypeExpr

--
-- SUBSTITUTION
--

newtype Subst = Subst (M.Map TypeVarName TypeExpr) deriving (Show)

emptySubst :: Subst
emptySubst = Subst M.empty

subst :: Subst -> TypeVarName -> TypeExpr
subst (Subst sub) tname =
  case M.lookup tname sub of
    Nothing -> TypeVar tname
    Just texpr -> texpr

applySubstType :: Subst -> TypeExpr -> TypeExpr
applySubstType sub (TypeVar tvar) = subst sub tvar
applySubstType sub (TypeCons tid ts) =
  TypeCons tid (map (applySubstType sub) ts)

--
-- M.union prefers `sub2'` when duplicate keys are encountered
composeSubst :: Subst -> Subst -> Subst
composeSubst s1@(Subst sub1) (Subst sub2) = Subst $ M.union sub2' sub1
 where
  sub2' = M.map (applySubstType s1) sub2

applySubstTypeList :: Subst -> [TypeExpr] -> [TypeExpr]
applySubstTypeList sub = fmap (applySubstType sub)


