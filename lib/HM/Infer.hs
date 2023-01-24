module HM.Infer where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import HM.Expr
import HM.NameSeed (
  NameSeed (..),
  increaseNsPrefix,
  startNameSeed,
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

applySubstTypeList :: Subst -> [TypeExpr] -> [TypeExpr]
applySubstTypeList sub = fmap (applySubstType sub)


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

typeExprFromScheme :: TypeScheme -> TypeExpr
typeExprFromScheme (Scheme _ texpr) = texpr


--
-- TYPE ENV
--

newtype TypeEnv = TypeEnv (M.Map VarName TypeScheme) deriving (Show)

emptyTypeEnv = TypeEnv M.empty

freeVarsTypeEnv :: TypeEnv -> [TypeVarName]
freeVarsTypeEnv (TypeEnv tenv) = concat (map freeVarsScheme (M.elems tenv))

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
  catMaybes
    $ fmap (\v -> fmap typeExprFromScheme $ M.lookup v tenv ) $ vars

keysTenv :: TypeEnv -> [VarName]
keysTenv (TypeEnv tenv) = M.keys tenv


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

data DataTypeEnv = DataTypeEnv (M.Map ConsName DataScheme) 
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

insertDataTypeEnv :: DataType -> [DataConsType] -> Env -> Env
insertDataTypeEnv (DataType tid tvnames) dataCons (Env t (DataTypeEnv denv) n) =
  let tscheme = Scheme (S.fromList tvnames) (TypeCons tid $ fmap TypeVar tvnames)
      consPairs = fmap (\(DataConsType cname texprs) -> (cname,texprs)) dataCons
      insertCons acc (name,texprs) = M.insert name (DataScheme tscheme texprs) acc
      denv' = foldl' insertCons denv consPairs
    in Env t (DataTypeEnv denv') n


--
-- ENV
--

type InferResult = Result (NameSeed, Subst, TypeExpr)

data Env = Env TypeEnv DataTypeEnv NameSeed

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

updateTypeEnv t' (Env _ d n) = Env t' d n
lookupTypeEnv var (Env (TypeEnv t) _ _) = M.lookup var t
insertVarTypeEnv pair (Env t d n) = Env (insertVarTypeEnv' pair t) d n

lookupDataEnv cname (Env _ (DataTypeEnv d) _) = M.lookup cname d

unionTypeEnv :: Env -> TypeEnv -> Env
unionTypeEnv (Env (TypeEnv t1) d n) (TypeEnv t2) = Env (TypeEnv $ M.union t2 t1) d n

isInTypeEnvScope :: VarName -> Env -> Bool
isInTypeEnvScope varn (Env (TypeEnv tenv) _ _) = M.member varn tenv

--
-- TYPE CHECKER
--
typeCheck :: Env -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheck env@(Env tenv _ ns) expr =
  case expr of
    Var tname -> typeCheckVar tenv ns tname
    Ap e1 e2 -> typeCheckAp env e1 e2
    Lambda x e -> typeCheckLambda env x e
    Let xs es e -> typeCheckLet env xs es e
    LetRec xs es e -> typeCheckLetRec env xs es e
    Case e pats es -> typeCheckCase env e pats es
    Guard cExpr gExprs exprs elseExpr -> typeCheckGuard env cExpr gExprs exprs elseExpr
    ELit lit -> typeCheckLit (nameSeed env) lit
    Adt cname es -> typeCheckDataType env cname es

typeCheckLit ns (LInt n) = Ok (ns, emptySubst, int)
typeCheckLit ns (LBool n) = Ok (ns, emptySubst, bool)

-- List expr
typeCheckList :: Env -> [Expr] -> Result (NameSeed, Subst, [TypeExpr])
typeCheckList tenv es = typeCheckList' tenv es $ Ok (emptySubst, [])

typeCheckList' :: Env -> [Expr] -> Result (Subst, [TypeExpr]) -> Result (NameSeed, Subst, [TypeExpr])
typeCheckList' env [] (Ok (sub, ts)) = Ok (nameSeed env, sub, reverse ts)
typeCheckList' _ _ (Fail f) = Fail f
typeCheckList' env (e : es) (Ok (sub, texprs)) =
  case (typeCheck env e) of
    Fail f -> Fail f
    Ok (ns', sub', texpr) ->
      let env' = updateNameSeed ns' . applySubstTypeEnv sub' $ env
          acc' = Ok (composeSubst sub sub', (applySubstType sub' texpr) : texprs)
       in typeCheckList' env' es acc'

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
typeCheckAp :: Env -> Expr -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckAp env e1 e2 =
  let ns = nameSeed env
      tname = TypeVarName $ varNameNs ns
      env' = increaseNameSeed env
   in case (typeCheckList env' [e1, e2]) of
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

typeCheckLambda :: Env -> VarName -> Expr -> InferResult
typeCheckLambda env var e =
  -- TODO: so boring, add the Functor instance
  case typeCheck env' e of
    Fail f -> Fail f
    Ok (ns'', sub, texpr) -> Ok (ns'', sub, arrow (subst sub tname) texpr)
 where
  tname = nextTypeVarName env
  env' = increaseNameSeed . insertVarTypeEnv (var, TypeVar tname) $ env

-- Let
typeCheckLet :: Env -> [VarName] -> [Expr] -> Expr -> InferResult
typeCheckLet env lvars rexprs expr =
  -- typecheck the right side
  case (typeCheckList env rexprs) of
    Fail f -> Fail f
    Ok (ns, subRexprs, tyRexprs) ->
      let env' = updateNameSeed ns . applySubstTypeEnv subRexprs $ env
          env'' = increaseNameSeed $ addDeclarations env' lvars tyRexprs
       in case typeCheck env'' expr of
            Fail f -> Fail f
            Ok (ns'', sub', texpr) -> Ok (ns'', composeSubst sub' subRexprs, texpr)

addDeclarations :: Env -> [VarName] -> [TypeExpr] -> Env 
addDeclarations (Env te@(TypeEnv tenv) d ns) vars texprs =
  Env (TypeEnv $ foldl' (\t (x, s) -> M.insert x s t) tenv (zip vars schemes)) d ns
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
typeCheckLetRec :: Env -> [VarName] -> [Expr] -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckLetRec env vars es expr =
  let newTenv = typeEnvFromVars (nameSeed env) vars
      -- a TypeEnv with the new vars/scheme
      env' = increaseNameSeed $ unionTypeEnv env newTenv
   in case (typeCheckList env' es) of
        Fail f -> Fail f
        Ok (ns2, sub, ts) ->
          let newTenv' = applySubstTypeEnv' sub newTenv
              ts' = typesFromVarsTenv vars newTenv'
              gamma' = applySubstTypeEnv sub env
           in case unifyEquations sub (zip ts ts') of
                Fail f -> Fail f
                (Ok sub') ->
                  let newTenv'' = applySubstTypeEnv' sub' newTenv'
                      ts'' = typesFromVarsTenv vars newTenv''
                      gamma'' = updateNameSeed ns2 $ applySubstTypeEnv sub' gamma'
                      gamma''' = increaseNameSeed $ addDeclarations gamma'' vars ts''
                   in case (typeCheck gamma''' expr) of
                        Fail f -> Fail f
                        Ok (ns4, sub'', texpr) -> Ok (ns4, composeSubst sub'' sub', texpr)

typeCheckDataType :: Env -> ConsName -> [Expr] -> Result (NameSeed, Subst, TypeExpr)
typeCheckDataType env consn exprs =
  case lookupDataEnv consn env of
    Nothing -> Fail $ "Hmmm constructor not found in the env; cons name: " ++ show consn
    Just dataScheme ->
      let (sub, dtexprs, dtexpr) = newDataInstance (nameSeed env) dataScheme
          env' = increaseNameSeed env
       in case typeCheckList env' exprs of
            Fail f -> Fail $ "Error when typecheking data type: " ++ f
            Ok (ns, sub1, texprs) ->
              let sub2 = composeSubst sub sub1 in
              case unifyEquations sub2 (zip texprs dtexprs) of
                Fail f -> Fail $ "Error when typechecking datatype: " ++ f
                Ok sub3 ->
                  let sub4 = composeSubst sub2 sub3
                   in Ok (ns, sub4, applySubstType sub4 dtexpr)

typeCheckCase :: Env -> Expr -> [Patn] -> [Expr] -> Result (NameSeed, Subst, TypeExpr)
typeCheckCase env expr pats exprs =
  case typeCheck env expr of
    Fail f -> Fail f
    Ok (ns', sub, texpr) ->
      case (zip pats exprs) of
        [] -> Fail "Missing case patterns"
        (x : xs) -> typeCheckCaseList env sub texpr xs (typeCheckOneCase (updateNameSeed ns' env) sub texpr x)

typeCheckCaseList :: Env -> Subst -> TypeExpr -> [(Patn, Expr)] -> InferResult -> InferResult
typeCheckCaseList _ _ _ [] acc = acc
typeCheckCaseList _ _ _ _ (Fail f) = Fail f
typeCheckCaseList env sub1 texpr (x : xs) (Ok (ns, sub2, texpr1)) =
  let sub3 = composeSubst sub1 sub2
   in case typeCheckOneCase (updateNameSeed ns env) sub1 texpr x of
        Fail f -> Fail f
        Ok (ns', sub4, texpr2) ->
          let sub5 = composeSubst sub3 sub4
           in case unify sub5 (texpr1, texpr2) of
                Fail f -> Fail f
                Ok sub6 -> typeCheckCaseList env sub1 texpr xs (Ok (ns', sub6, texpr2))

typeCheckOneCase :: Env -> Subst -> TypeExpr -> (Patn, Expr) -> Result (NameSeed, Subst, TypeExpr)
typeCheckOneCase env sub texpr (patn, expr) =
  case typeCheckPatn False env sub patn of
    Fail f -> Fail f
    Ok (envPat, sub', tpat) ->
      case unify (composeSubst sub sub') (tpat, texpr) of
        Fail f -> Fail f
        Ok sub'' ->
          let env' = increaseNameSeed $ applySubstTypeEnv sub'' envPat
           in typeCheck env' expr

-- TODO: when a variable appears more then on time at the same pattern
typeCheckPatn :: Bool -> Env -> Subst -> Patn -> Result (Env, Subst, TypeExpr)
typeCheckPatn isPatScope env sub pat =
  case pat of
    PatnVar varn ->
      let tvar = typeVar $ nextNameSeed env
          scheme = Scheme S.empty tvar
          env' = unionTypeEnv env $ TypeEnv $ M.singleton varn scheme
          checkScope = if isPatScope then isInTypeEnvScope varn env else False
       in case checkScope of
            False -> Ok (env', sub, tvar)
            True  -> Fail $ "Variable " ++ show varn ++ " already bound"
    PatnLit lit ->
      case typeCheckLit (nameSeed env) lit of
        Ok (_, sub', texpr) -> Ok (env, composeSubst sub sub', texpr)
        _ -> Fail $ "Unexpected error when typechecking patnlit " ++ show pat
    PatnHole -> Ok (env, sub, typeVar $ nextNameSeed env)
    PatnCons (Cons cname patns) ->
      case lookupDataEnv cname env of
        Just dataScheme ->
          let (dsub, dtexprs, dtexpr) = newDataInstance (nameSeed env) dataScheme
              env' = increaseNameSeed env
              resultPatns = case patns of
                              [] -> Ok (env', sub, [])
                              (x:xs) -> typeCheckPatnList xs (typeCheckPatn True env' sub x) []
           in case resultPatns of
                Ok (env'', sub2, texprs') ->
                  let sub3 = composeSubst sub2 dsub in
                  case unifyEquations sub3 (zip texprs' dtexprs) of
                    Ok sub4 -> Ok (env'', sub4, applySubstType sub4 dtexpr)
                    Fail f -> Fail $ "Unexpected error when typechecking patncons " ++ show pat ++ ": " ++ f
                Fail f -> Fail f
        Nothing -> Fail $ "Hmmm constructor not found in the env; cons name: " ++ show cname

typeCheckPatnList [] (Ok (env, sub, texp)) acc = Ok (env, sub, reverse $ texp:acc)
typeCheckPatnList _ (Fail f) _ = Fail f
typeCheckPatnList (p:ps) (Ok (env, sub, texpr)) acc =
  typeCheckPatnList ps (typeCheckPatn True env sub p) (texpr:acc)

--- Guard
typeCheckGuard :: Env -> Expr -> [Expr] -> [Expr] -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckGuard env condExpr guardExprs exprs elseExpr =
  case typeCheck env condExpr of
    Fail f -> Fail f
    Ok (env',sub,texpr) ->
      case (zip guardExprs exprs) of
        [] -> Fail "Missing guard expressions"
        (x:xs) -> 
          let 
            fmapTexpr = everyOrFail . fmap (mapResult (\(_,_,t) -> t) . typeCheck env)
            tGuardExprs = fmapTexpr guardExprs
            tExprs = fmapTexpr (exprs ++ [elseExpr])

            unifyResult tpe tpes = constResult tpe $ unifyListBy sub tpe tpes

            rGuardExprs = flattenResult $ mapResult (unifyResult texpr) tGuardExprs
            rExprs = flattenResult $ mapResult (\(e:es) -> unifyResult e es) tExprs

            ans = continueOrFail rGuardExprs rExprs
          in
            case ans of
              Fail f -> Fail f
              Ok (texpr') -> Ok (nameSeed env, emptySubst, texpr')
            
unifyListBy :: Subst -> TypeExpr -> [TypeExpr] -> Result Subst
unifyListBy sub tpe (t:ts) = foldl' (\acc c -> continueOrFail acc $ uni c) (uni t) ts
  where uni a = unify sub (tpe, a)

everyOrFail :: [Result a] -> Result [a]
everyOrFail (x:xs) = mapResult reverse $ foldl' accOrFail x' xs
  where x' = mapResult (\a -> [a]) x

accOrFail ff@(Fail f) _ = Fail f
accOrFail (Ok acc) (Ok c) = Ok (c:acc)
accOrFail (Ok _) ff@(Fail f) = Fail f

continueOrFail ff@(Fail f) _ = ff
continueOrFail (Ok _) r = r

mapResult _ (Fail e) = Fail e
mapResult g (Ok a) = Ok $ g a
            
flattenResult (Ok (Ok a)) = Ok a
flattenResult (Ok (Fail f)) = Fail f
flattenResult (Fail f) = Fail f

constResult a = mapResult (const a)

