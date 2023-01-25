module HM.Infer where

import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import HM.Env
import HM.Env.DataTypeEnv
import HM.Env.TypeEnv
import HM.Expr
import HM.NameSeed
import HM.Result
import HM.Type.Ops
import HM.Type.Subst
import HM.Type.TypeExpr
import HM.Type.TypeScheme
import HM.Type.Unification

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

typeCheckLit :: a -> Lit -> Result (a, Subst, TypeExpr)
typeCheckLit ns LInt = Ok (ns, emptySubst, int)

-- List expr
typeCheckList :: Env -> [Expr] -> Result (NameSeed, Subst, [TypeExpr])
typeCheckList tenv es = typeCheckList' tenv es $ Ok (emptySubst, [])

typeCheckList' :: Env -> [Expr] -> Result (Subst, [TypeExpr]) -> Result (NameSeed, Subst, [TypeExpr])
typeCheckList' env [] (Ok (sub, ts)) = Ok (nameSeed env, sub, reverse ts)
typeCheckList' _ _ (Fail f) = Fail f
typeCheckList' env (e : es) (Ok (sub, texprs)) =
  case typeCheck env e of
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

-- AP
typeCheckAp :: Env -> Expr -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckAp env e1 e2 =
  let ns = nameSeed env
      tname = TypeVarName $ varNameNs ns
      env' = increaseNameSeed env
   in case typeCheckList env' [e1, e2] of
        Fail f -> Fail f
        Ok (ns'', sub, [t1, t2]) ->
          -- (AP e1 e2):t'
          -- e1:t1; e2:t2
          -- t1 unify (t2 -> t')
          case unify sub (t1, arrow t2 (TypeVar tname)) of
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
  case typeCheckList env rexprs of
    Fail f -> Fail f
    Ok (ns, subRexprs, tyRexprs) ->
      let env' = updateNameSeed ns . applySubstTypeEnv subRexprs $ env
          env'' = increaseNameSeed $ addDeclarations env' lvars tyRexprs
       in case typeCheck env'' expr of
            Fail f -> Fail f
            Ok (ns'', sub', texpr) -> Ok (ns'', composeSubst sub' subRexprs, texpr)

-- Let Rec
typeCheckLetRec :: Env -> [VarName] -> [Expr] -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckLetRec env vars es expr =
  let newTenv = typeEnvFromVars (nameSeed env) vars
      -- a TypeEnv with the new vars/scheme
      env' = increaseNameSeed $ unionTypeEnv env newTenv
   in case typeCheckList env' es of
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
                   in case typeCheck gamma''' expr of
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
              let sub2 = composeSubst sub sub1
               in case unifyEquations sub2 (zip texprs dtexprs) of
                    Fail f -> Fail $ "Error when typechecking datatype: " ++ f
                    Ok sub3 ->
                      let sub4 = composeSubst sub2 sub3
                       in Ok (ns, sub4, applySubstType sub4 dtexpr)

typeCheckCase :: Env -> Expr -> [Patn] -> [Expr] -> Result (NameSeed, Subst, TypeExpr)
typeCheckCase env expr pats exprs =
  case typeCheck env expr of
    Fail f -> Fail f
    Ok (ns', sub, texpr) ->
      case zip pats exprs of
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
          checkScope = isPatScope && isInTypeEnvScope varn env
       in case checkScope of
            False -> Ok (env', sub, tvar)
            True -> Fail $ "Variable " ++ show varn ++ " already bound"
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
                (x : xs) -> typeCheckPatnList xs (typeCheckPatn True env' sub x) []
           in case resultPatns of
                Ok (env'', sub2, texprs') ->
                  let sub3 = composeSubst sub2 dsub
                   in case unifyEquations sub3 (zip texprs' dtexprs) of
                        Ok sub4 -> Ok (env'', sub4, applySubstType sub4 dtexpr)
                        Fail f -> Fail $ "Unexpected error when typechecking patncons " ++ show pat ++ ": " ++ f
                Fail f -> Fail f
        Nothing -> Fail $ "Hmmm constructor not found in the env; cons name: " ++ show cname

typeCheckPatnList ::
  [Patn] ->
  Result (Env, Subst, TypeExpr) ->
  [TypeExpr] ->
  Result (Env, Subst, [TypeExpr])
typeCheckPatnList [] (Ok (env, sub, texp)) acc = Ok (env, sub, reverse $ texp : acc)
typeCheckPatnList _ (Fail f) _ = Fail f
typeCheckPatnList (p : ps) (Ok (env, sub, texpr)) acc =
  typeCheckPatnList ps (typeCheckPatn True env sub p) (texpr : acc)

--- Guard
typeCheckGuard :: Env -> Expr -> [Expr] -> [Expr] -> Expr -> Result (NameSeed, Subst, TypeExpr)
typeCheckGuard env condExpr guardExprs exprs elseExpr =
  case typeCheck env condExpr of
    Fail f -> Fail f
    Ok (_, sub, texpr) ->
      case zip guardExprs exprs of
        [] -> Fail "Missing guard expressions"
        _ ->
          let fmapTexpr = everyOrFail . fmap (mapResult (\(_, _, t) -> t) . typeCheck env)
              tGuardExprs = fmapTexpr guardExprs
              tExprs = fmapTexpr (exprs ++ [elseExpr])

              unifyResult tpe tpes = constResult tpe $ unifyListBy sub tpe tpes

              rGuardExprs = flattenResult $ mapResult (unifyResult texpr) tGuardExprs
              rExprs = flattenResult $ mapResult (\(e : es) -> unifyResult e es) tExprs

              ans = continueOrFail rGuardExprs rExprs
           in case ans of
                Fail f -> Fail f
                Ok texpr' -> Ok (nameSeed env, emptySubst, texpr')
