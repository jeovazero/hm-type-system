module HM.Type.Ops where

import qualified Data.Set as S
import qualified Data.Map as M
import HM.Type.TypeExpr
import HM.Type.TypeScheme
import HM.NameSeed
import HM.Result
import HM.Type.Subst
import HM.Type.Unification
import Data.List

generalization :: S.Set TypeVarName -> NameSeed -> TypeExpr -> TypeScheme
generalization freeVars ns texpr =
  Scheme (S.fromList $ map (\(TypeVar x) -> x) $ M.elems substMap) texpr'
 where
  snames = filter (\v -> not (S.member v freeVars)) (typeVarsIn texpr)
  substMap = M.fromList $ zip snames tnamesSequence
  sub = Subst substMap
  tnamesSequence = map (TypeVar . TypeVarName) $ varNameSequence ns
  texpr' = applySubstType sub texpr

newInstance :: NameSeed -> TypeScheme -> TypeExpr
newInstance ns (Scheme snames texpr) = applySubstType sub texpr
 where
  sub = Subst . M.fromList $ zip (S.toList snames) tnamesSequence
  tnamesSequence = map (TypeVar . TypeVarName) $ varNameSequence ns -- todo: rename

unifyListBy :: Subst -> TypeExpr -> [TypeExpr] -> Result Subst
unifyListBy sub tpe (t:ts) = foldl' (\acc c -> continueOrFail acc $ uni c) (uni t) ts
  where uni a = unify sub (tpe, a)

