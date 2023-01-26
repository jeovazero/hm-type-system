{-# LANGUAGE OverloadedStrings #-}

module HM where

import HM.Expr
import HM.Infer
import HM.Pretty
import HM.Result
import HM.Env
import HM.Env.TypeEnv
import HM.Env.DataTypeEnv
import HM.Type.TypeExpr
import qualified Data.Map as M
import HM.Type.TypeScheme

varn = VarName

var = Var . VarName

letrec vs es e = LetRec (map varn vs) es e

lam v e = Lambda (varn v) e

ap e1 e2 = Ap e1 e2

litInt = ELit LInt

lamcase e p es = Case e p es

lamguard e ges es elseE = Guard e ges es elseE

patvar = PatnVar

pathole = PatnHole

patlit = PatnLit

patcons name = PatnCons name

adt name es = Adt name es
cons name args = Cons name args
consName = ConsName 
dataType = DataType
consType = DataConsType
tid = TIdentifier
tVarName = TypeVarName
initEnv = startEnv

aliasTypeEnv :: TypeEnv -> M.Map VarName String
aliasTypeEnv (TypeEnv tenvMap) = M.map (aliasTypevar . typeExprFromScheme) tenvMap

runInferEnv :: Env -> Expr -> Either String (Env, TypeExpr)
runInferEnv env expr =
  case typeCheck env expr of
    Ok (renv, _, t) -> Right (renv, t)
    Fail f -> Left f

