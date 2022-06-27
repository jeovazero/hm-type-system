module HM.Infer where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import HM.Expr
import HM.NameSeed (
  NameSeed (..),
  increaseNsIndex,
  splitNsPrefix,
  varNameNs,
  varNameSequence,
 )
import HM.TypeExpr

--
-- SUBSTITUTION
--

newtype Subst = Subst (M.Map TypeVarName TypeExpr)

emptySubst = Subst $ M.empty

subst :: Subst -> TypeVarName -> TypeExpr
subst (Subst sub) tname =
  case M.lookup tname sub of
    Nothing -> TypeVar tname
    Just texpr -> texpr

applySubstType :: Subst -> TypeExpr -> TypeExpr
applySubstType sub t@(TypeVar tvar) = subst sub tvar
applySubstType sub (TypeCons tid ts) =
  TypeCons tid (map (applySubstType sub) ts)

--
-- M.union prefers `sub2'` when duplicate keys are encountered
composeSubst :: Subst -> Subst -> Subst
composeSubst s1@(Subst sub1) (Subst sub2) = Subst $ M.union sub2' sub1
 where
  sub2' = (M.map (applySubstType s1) sub2)

--
-- UNIFICATION
--

data Result a = Ok a | Fail String deriving (Show)

typeVarsIn :: TypeExpr -> [TypeVarName]
typeVarsIn texpr = typeVarsIn' texpr []
 where
  typeVarsIn' (TypeVar tname) acc = tname : acc
  typeVarsIn' (TypeCons tid ts) acc = foldl' (flip typeVarsIn') acc ts

extend :: Subst -> TypeVarName -> TypeExpr -> Result Subst
extend sub tname texpr
  | texpr == TypeVar tname = Ok sub
  | tname `elem` (typeVarsIn texpr) =
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
unify sub (ta@(TypeCons tid ts), tb@(TypeVar tname)) = unify sub (tb, ta)
unify sub (ta@(TypeCons tid1 ts1), tb@(TypeCons tid2 ts2))
  | tid1 == tid2 = unifyEquations sub (zip ts1 ts2)
  | otherwise = Fail $ concat ["Cannot unify ", show ta, " and ", show tb]

unifyEquations :: Subst -> [(TypeExpr, TypeExpr)] -> Result Subst
unifyEquations sub equations =
  foldl' unify' (Ok sub) equations
 where
  unify' (Ok sub) equation = unify sub equation
  unify' f@(Fail _) _ = f

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

--
-- TYPE ENV
--

newtype TypeEnv = TypeEnv (M.Map VarName TypeScheme)

freeVarsTypeEnv :: TypeEnv -> [TypeVarName]
freeVarsTypeEnv (TypeEnv tenv) = concat (map freeVarsScheme (M.elems tenv))

applySubstTypeEnv :: Subst -> TypeEnv -> TypeEnv
applySubstTypeEnv sub (TypeEnv tenv) = TypeEnv $ M.map (applySubstScheme sub) tenv

typeEnvFromVars :: NameSeed -> [VarName] -> TypeEnv
typeEnvFromVars ns vars =
  let schemeVars = zipVarsScheme vars ns
   in TypeEnv $ M.fromList schemeVars

unionTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
unionTypeEnv (TypeEnv te1) (TypeEnv te2) = TypeEnv $ M.union te2 te1

typesFromVarsTenv (TypeEnv tenv) = map (\(Scheme _ t) -> t) $ M.elems tenv

keysTenv (TypeEnv tenv) = M.keys tenv

--
-- TYPE CHECKER
--
typeCheck :: TypeEnv -> NameSeed -> Expr -> Result (Subst, TypeExpr)
typeCheck tenv ns expr =
  case expr of
    Var tname -> typeCheckVar tenv ns tname
    Ap e1 e2 -> typeCheckAp tenv ns e1 e2
    Lambda x e -> typeCheckLambda tenv ns x e
    Let xs es e -> typeCheckLet tenv ns xs es e
    LetRec xs es e -> typeCheckLetRec tenv ns xs es e
    ELit lit -> typeCheckLit lit

typeCheckLit (LInt n) = Ok (emptySubst, int)
typeCheckLit (LBool n) = Ok (emptySubst, bool)

-- List expr
typeCheckList :: TypeEnv -> NameSeed -> [Expr] -> Result (Subst, [TypeExpr])
typeCheckList tenv ns es = typeCheckList' tenv ns es $ Ok (emptySubst, [])

typeCheckList' :: TypeEnv -> NameSeed -> [Expr] -> Result (Subst, [TypeExpr]) -> Result (Subst, [TypeExpr])
typeCheckList' tenv ns [] acc@(Ok (sub, ts)) = Ok (sub, reverse ts)
typeCheckList' tenv ns _ acc@(Fail _) = acc
typeCheckList' tenv ns (e : es) acc@(Ok (sub, texprs)) =
  let (ns1, ns2) = splitNsPrefix ns
   in case (typeCheck tenv ns2 e) of
        Fail f -> Fail f
        Ok (sub', texpr) ->
          let tenv' = applySubstTypeEnv sub' tenv
              acc' = Ok (composeSubst sub sub', (applySubstType sub' texpr) : texprs)
           in typeCheckList' tenv' ns1 es acc'

-- Var
typeCheckVar :: TypeEnv -> NameSeed -> VarName -> Result (Subst, TypeExpr)
typeCheckVar (TypeEnv tenv) ns tvar =
  case M.lookup tvar tenv of
    Just scheme -> Ok (emptySubst, newInstance ns scheme)
    Nothing -> Fail $ "unbound variable: " ++ show tvar

newInstance :: NameSeed -> TypeScheme -> TypeExpr
newInstance ns (Scheme snames texpr) = applySubstType sub texpr
 where
  sub = Subst . M.fromList $ zip (S.toList snames) tnamesSequence
  tnamesSequence = map (TypeVar . TypeVarName) $ varNameSequence ns -- todo: rename

-- AP
typeCheckAp :: TypeEnv -> NameSeed -> Expr -> Expr -> Result (Subst, TypeExpr)
typeCheckAp gamma ns e1 e2 =
  let tname = TypeVarName $ varNameNs ns
      ns' = increaseNsIndex ns
   in case (typeCheckList gamma ns' [e1, e2]) of
        Fail f -> Fail f
        Ok (sub, [t1, t2]) ->
          -- (AP e1 e2):t'
          -- e1:t1; e2:t2
          -- t1 unify (t2 -> t')
          case (unify sub (t1, arrow t2 (TypeVar tname))) of
            Fail f -> Fail f
            Ok sub' -> Ok (sub', subst sub' tname) -- t'
        _ -> Fail $ "Unexpected error: expected typeCheckList " ++ show [e1, e2]

-- Lambda

typeCheckLambda (TypeEnv tenv) ns x e =
  -- TODO: so boring, add the Functor instance
  case typeCheck tenv' ns' e of
    Fail f -> Fail f
    Ok (sub, texpr) -> Ok (sub, arrow (subst sub tname) texpr)
 where
  ns' = increaseNsIndex ns
  tenv' = TypeEnv $ M.insert x (Scheme S.empty (TypeVar tname)) tenv
  tname = TypeVarName $ varNameNs ns

-- Let
typeCheckLet tenv nameSeed lvars rexprs expr =
  let (ns0, ns1) = splitNsPrefix nameSeed
   in -- typecheck the right side
      case (typeCheckList tenv ns1 rexprs) of
        Fail f -> Fail f
        Ok (subRexprs, tyRexprs) ->
          let (ns2, ns3) = splitNsPrefix ns0
              tenv'' = addDeclarations tenv' ns2 lvars tyRexprs
              tenv' = applySubstTypeEnv subRexprs tenv
           in case typeCheck tenv'' ns3 expr of
                Fail f -> Fail f
                Ok (sub', texpr) -> Ok (composeSubst sub' subRexprs, texpr)

addDeclarations te@(TypeEnv tenv) ns vars texprs =
  TypeEnv $ foldl' (\t (x, s) -> M.insert x s t) tenv (zip vars schemes)
 where
  schemes = map (generalization freeVars ns) texprs
  freeVars = S.fromList $ freeVarsTypeEnv te

generalization freeVars ns texpr =
  Scheme (S.fromList $ map (\(TypeVar x) -> x) $ M.elems substMap) texpr'
 where
  snames = filter (\v -> not (S.member v freeVars)) (typeVarsIn texpr)
  substMap = M.fromList $ zip snames tnamesSequence
  sub = Subst substMap
  tnamesSequence = map (TypeVar . TypeVarName) $ varNameSequence ns
  texpr' = applySubstType sub texpr

-- Let Rec
typeCheckLetRec :: TypeEnv -> NameSeed -> [VarName] -> [Expr] -> Expr -> Result (Subst, TypeExpr)
typeCheckLetRec tenv ns vars es expr =
  let newTenv = typeEnvFromVars ns3 vars
      -- a TypeEnv with the new vars/scheme
      tenv' = unionTypeEnv tenv newTenv
      (ns0, ns1) = splitNsPrefix ns
      (ns2, ns3) = splitNsPrefix ns1
   in case (typeCheckList tenv' ns2 es) of
        Fail f -> Fail f
        Ok (sub, ts) ->
          let newTenv' = applySubstTypeEnv sub newTenv
              ts' = typesFromVarsTenv newTenv'
              gamma' = applySubstTypeEnv sub tenv
           in case unifyEquations sub (zip ts ts') of
                Fail f -> Fail f
                (Ok sub') ->
                  let newTenv'' = applySubstTypeEnv sub' newTenv'
                      ts'' = typesFromVarsTenv newTenv''
                      gamma'' = applySubstTypeEnv sub' gamma'
                      gamma''' = addDeclarations gamma'' ns3 (keysTenv newTenv') ts''
                      (ns3, ns4) = splitNsPrefix ns
                   in case (typeCheck gamma''' ns4 expr) of
                        Fail f -> Fail f
                        Ok (sub'', texpr) -> Ok (composeSubst sub'' sub', texpr)
