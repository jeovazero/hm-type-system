module HM.Type.TypeScheme where

import qualified Data.Set as S
import qualified Data.Map as M
import HM.Type.TypeExpr
import HM.Type.Subst
import HM.NameSeed
import HM.Expr
import Data.List

--
-- TYPE SCHEME
--

data TypeScheme = Scheme (S.Set TypeVarName) TypeExpr deriving (Show)

freeVarsScheme :: TypeScheme -> [TypeVarName]
freeVarsScheme (Scheme snames texpr) =
  filter (\v -> not (S.member v snames)) (typeVarsIn texpr)

applySubstScheme :: Subst -> TypeScheme -> TypeScheme
applySubstScheme (Subst sub) (Scheme snames texpr) =
  Scheme snames (applySubstType sub' texpr)
 where
  -- to ensure a subst with only non schematic vars
  sub' = Subst $ foldl' (flip M.delete) sub snames

zipVarsScheme :: [VarName] -> NameSeed -> [(VarName, TypeScheme)]
zipVarsScheme vars ns =
  zip vars $ map (\x -> Scheme S.empty (TypeVar $ TypeVarName x)) $ varNameSequence ns

typeExprFromScheme :: TypeScheme -> TypeExpr
typeExprFromScheme (Scheme _ texpr) = texpr


