module HM.Infer where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import HM.Expr
import HM.NameSeed (
  NameSeed (..),
  increaseNsPrefix,
  varNameNs,
  varNameSequence,
  zipWithNs,
 )
import Data.Maybe (catMaybes)
import HM.TypeExpr
import Debug.Trace

--
-- SUBSTITUTION
--

newtype Subst = Subst (M.Map TypeVarName TypeExpr) deriving (Show)

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

newtype TypeEnv = TypeEnv (M.Map VarName TypeScheme) deriving (Show)

emptyTypeEnv = TypeEnv M.empty

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

typesFromVarsTenv vars (TypeEnv tenv) = catMaybes $ fmap (\v -> fmap (\(Scheme _ t) -> t) $ M.lookup v tenv ) $ vars

keysTenv (TypeEnv tenv) = M.keys tenv

--
-- TYPE CHECKER
--
typeCheck :: TypeEnv -> NameSeed -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheck tenv ns expr =
  case expr of
    Var tname -> typeCheckVar tenv ns tname
    Ap e1 e2 -> typeCheckAp tenv ns e1 e2
    Lambda x e -> typeCheckLambda tenv ns x e
    Let xs es e -> typeCheckLet tenv ns xs es e
    LetRec xs es e -> typeCheckLetRec tenv ns xs es e
    Case e pats es -> typeCheckCase tenv ns e pats es
    ELit lit -> typeCheckLit ns lit

typeCheckLit ns (LInt n) = Ok (ns, emptySubst, int)
typeCheckLit ns (LBool n) = Ok (ns, emptySubst, bool)

-- List expr
typeCheckList :: TypeEnv -> NameSeed -> [Expr] -> Result (NameSeed, Subst, [TypeExpr])
typeCheckList tenv ns es = typeCheckList' tenv ns es $ Ok (emptySubst, [])

typeCheckList' :: TypeEnv -> NameSeed -> [Expr] -> Result (Subst, [TypeExpr]) -> Result (NameSeed, Subst, [TypeExpr])
typeCheckList' _ ns [] (Ok (sub, ts)) = Ok (ns, sub, reverse ts)
typeCheckList' _ _ _ (Fail f) = Fail f
typeCheckList' tenv ns (e : es) (Ok (sub, texprs)) =
  case (typeCheck tenv ns e) of
    Fail f -> Fail f
    Ok (ns', sub', texpr) ->
      let tenv' = applySubstTypeEnv sub' tenv
          acc' = Ok (composeSubst sub sub', (applySubstType sub' texpr) : texprs)
       in typeCheckList' tenv' ns' es acc'

-- Var
typeCheckVar :: TypeEnv -> NameSeed -> VarName -> Result (NameSeed, Subst, TypeExpr)
typeCheckVar (TypeEnv tenv) ns tvar =
  case M.lookup tvar tenv of
    Just scheme -> Ok (increaseNsPrefix ns, emptySubst, newInstance ns scheme)
    Nothing -> Fail $ "unbound variable: " ++ show tvar

newInstance :: NameSeed -> TypeScheme -> TypeExpr
newInstance ns (Scheme snames texpr) = applySubstType sub texpr
 where
  sub = Subst . M.fromList $ zip (S.toList snames) tnamesSequence
  tnamesSequence = map (TypeVar . TypeVarName) $ varNameSequence ns -- todo: rename

-- AP
typeCheckAp :: TypeEnv -> NameSeed -> Expr -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckAp gamma ns e1 e2 =
  let tname = TypeVarName $ varNameNs ns
      ns' = increaseNsPrefix ns
   in case (typeCheckList gamma ns' [e1, e2]) of
        Fail f -> Fail f
        Ok (ns'', sub, [t1, t2]) ->
          -- (AP e1 e2):t'
          -- e1:t1; e2:t2
          -- t1 unify (t2 -> t')
          case (unify sub (t1, arrow t2 (TypeVar tname))) of
            Fail f -> Fail f
            Ok sub' -> Ok (ns'', sub', subst sub' tname) -- t'
        _ -> Fail $ "Unexpected error: expected typeCheckList " ++ show [e1, e2]

-- Lambda

typeCheckLambda (TypeEnv tenv) ns x e =
  -- TODO: so boring, add the Functor instance
  case typeCheck tenv' ns' e of
    Fail f -> Fail f
    Ok (ns'', sub, texpr) -> Ok (ns'', sub, arrow (subst sub tname) texpr)
 where
  ns' = increaseNsPrefix ns
  tenv' = TypeEnv $ M.insert x (Scheme S.empty (TypeVar tname)) tenv
  tname = TypeVarName $ varNameNs ns

-- Let
typeCheckLet tenv nameSeed lvars rexprs expr =
  -- typecheck the right side
  case (typeCheckList tenv nameSeed rexprs) of
    Fail f -> Fail f
    Ok (ns, subRexprs, tyRexprs) ->
      let ns' = increaseNsPrefix ns
          tenv'' = addDeclarations tenv' ns lvars tyRexprs
          tenv' = applySubstTypeEnv subRexprs tenv
       in case typeCheck tenv'' ns' expr of
            Fail f -> Fail f
            Ok (ns'', sub', texpr) -> Ok (ns'', composeSubst sub' subRexprs, texpr)

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
typeCheckLetRec :: TypeEnv -> NameSeed -> [VarName] -> [Expr] -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckLetRec tenv ns vars es expr =
  let newTenv = typeEnvFromVars ns vars
      -- a TypeEnv with the new vars/scheme
      tenv' = unionTypeEnv tenv newTenv
      ns1 = increaseNsPrefix ns
   in case (typeCheckList tenv' ns1 es) of
        Fail f -> Fail f
        Ok (ns2, sub, ts) ->
          let newTenv' = applySubstTypeEnv sub newTenv
              ts' = typesFromVarsTenv vars newTenv'
              gamma' = applySubstTypeEnv sub tenv
           in case unifyEquations sub (zip ts ts') of
                Fail f -> Fail f
                (Ok sub') ->
                  let newTenv'' = applySubstTypeEnv sub' newTenv'
                      ts'' = typesFromVarsTenv vars newTenv''
                      gamma'' = applySubstTypeEnv sub' gamma'
                      gamma''' = addDeclarations gamma'' ns2 vars ts''
                      ns3 = increaseNsPrefix ns2
                   in case (typeCheck gamma''' ns3 expr) of
                        Fail f -> Fail f
                        Ok (ns4, sub'', texpr) -> Ok (ns4, composeSubst sub'' sub', texpr)

typeCheckCase :: TypeEnv -> NameSeed -> Expr -> [Patn] -> [Expr] -> Result (NameSeed, Subst, TypeExpr)
typeCheckCase tenv ns expr pats exprs =
  case typeCheck tenv ns expr of
    Fail f -> Fail f
    Ok (ns', sub, texpr) ->
      case (zip pats exprs) of
        [] -> Fail "Missing case patterns"
        (x : xs) -> typeCheckCaseList tenv sub texpr xs (typeCheckOneCase tenv ns' sub texpr x)

type InferResult = Result (NameSeed, Subst, TypeExpr)

typeCheckCaseList :: TypeEnv -> Subst -> TypeExpr -> [(Patn, Expr)] -> InferResult -> InferResult
typeCheckCaseList _ _ _ [] acc = acc
typeCheckCaseList _ _ _ _ (Fail f) = Fail f
typeCheckCaseList tenv sub1 texpr (x : xs) (Ok (ns, sub2, texpr1)) =
  let sub3 = composeSubst sub1 sub2
   in case typeCheckOneCase tenv ns sub1 texpr x of
        Fail f -> Fail f
        Ok (ns', sub4, texpr2) ->
          let sub5 = composeSubst sub3 sub4
           in case unify sub5 (texpr1, texpr2) of
                Fail f -> Fail f
                Ok sub6 -> typeCheckCaseList tenv sub1 texpr xs (Ok (ns', sub6, texpr2))

typeCheckOneCase :: TypeEnv -> NameSeed -> Subst -> TypeExpr -> (Patn, Expr) -> Result (NameSeed, Subst, TypeExpr)
typeCheckOneCase tenv ns sub texpr (patn, expr) =
  case typeCheckPatn ns patn of
    Fail f -> Fail f
    Ok (tenvPat, sub', tpat) ->
      case unify (composeSubst sub sub') (tpat, texpr) of
        Fail f -> Fail f
        Ok sub'' ->
          let tenv' = unionTypeEnv tenv tenvPat
              tenv'' = applySubstTypeEnv sub'' tenv'
              ns' = increaseNsPrefix ns
           in typeCheck tenv'' ns' expr

typeCheckPatn :: NameSeed -> Patn -> Result (TypeEnv, Subst, TypeExpr)
typeCheckPatn ns pat =
  case pat of
    PatnVar varn ->
      let tvar = typeVar $ varNameNs ns
          scheme = Scheme S.empty tvar
          te = TypeEnv $ M.singleton varn scheme
       in Ok (te, emptySubst, tvar)
    PatnLit lit ->
      case typeCheckLit ns lit of
        Ok (_, sub, texpr) -> Ok (emptyTypeEnv, sub, texpr)
        _ -> Fail $ "Unexpected error when typechecking patnlit " ++ show pat
    PatnHole -> Ok (emptyTypeEnv, emptySubst, typeVar $ varNameNs ns)
