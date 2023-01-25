{-# LANGUAGE OverloadedStrings #-}

module HM where

import HM.Expr
import HM.Infer
import HM.Pretty
import HM.Result
import HM.Env
import HM.Env.DataTypeEnv
import HM.Type.TypeExpr

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
tvar = TypeVarName
initEnv = startEnv

runInfer = runInferEnv startEnv
runInferEnv env expr =
  case typeCheck env expr of
    Ok (_, _, t) -> do
        putStrLn .
          concat $ [ prettyExpr expr
                   , "\n-------------------\n"
                   , "Infer: "
                   , aliasTypevar t
                   , "\n"
                   ]
    Fail f ->
      putStrLn .
        concat $ [ prettyExpr expr
                 , "\n-------------------\n"
                 , "Error: "
                 , f
                 , "\n"
                 ]
