{-# LANGUAGE OverloadedStrings #-}

module HM where

import HM.Expr
import HM.Infer
import HM.Pretty
import HM.TypeExpr

varn = VarName

var = Var . VarName

letrec vs es e = LetRec (map varn vs) es e

lam v e = Lambda (varn v) e

ap e1 e2 = Ap e1 e2

litInt = ELit . LInt

litBool = ELit . LBool

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
    Ok (_, s, t) -> do
        print s
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
