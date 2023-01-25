module HM.Type.Unification where

import HM.Type.TypeExpr
import HM.Type.Subst
import HM.Result
import qualified Data.Map as M
import Data.List


--
-- UNIFICATION
--

extend :: Subst -> TypeVarName -> TypeExpr -> Result Subst
extend sub tname texpr
  | texpr == TypeVar tname = Ok sub
  | tname `elem` typeVarsIn texpr =
      -- Eg.: a1 with [a1]; a1 with b2 -> a1
      Fail $ concat ["Impossible to extend ", show tname, " with ", show texpr]
  | otherwise = Ok $ composeSubst (Subst $ M.singleton tname texpr) sub

unify :: Subst -> (TypeExpr, TypeExpr) -> Result Subst
unify sub (TypeVar tname, texpr)
  -- there is no subst type, then extend
  | subTexpr == TypeVar tname = extend sub tname texpr'
  -- unify the found subst type
  | otherwise = unify sub (subTexpr, texpr')
 where
  subTexpr = subst sub tname
  texpr' = applySubstType sub texpr
unify sub (ta@(TypeCons _ _), tb@(TypeVar _)) = unify sub (tb, ta)
unify sub (ta@(TypeCons tid1 ts1), tb@(TypeCons tid2 ts2))
  | tid1 == tid2 = unifyEquations sub (zip ts1 ts2)
  | otherwise = Fail $ concat ["Cannot unify ", show ta, " and ", show tb]

unifyEquations :: Subst -> [(TypeExpr, TypeExpr)] -> Result Subst
unifyEquations sub = foldl' unify' (Ok sub)
 where
  unify' (Ok sub') equation = unify sub' equation
  unify' f@(Fail _) _ = f

