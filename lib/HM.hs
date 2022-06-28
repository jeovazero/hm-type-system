{-# LANGUAGE OverloadedStrings #-}

module HM where

import qualified Data.Map as M
import HM.Expr
import HM.Infer
import HM.NameSeed (startNameSeed)
import HM.Pretty

varn = VarName

var = Var . VarName

letrec vs es e = LetRec (map varn vs) es e

lam v e = Lambda (varn v) e

ap e1 e2 = Ap e1 e2

litInt = ELit . LInt

litBool = ELit . LBool

lamcase e p es = Case e p es

patvar = PatnVar

pathole = PatnHole

patlit = PatnLit

runInfer expr =
  case typeCheck (TypeEnv M.empty) (startNameSeed) expr of
    Ok (_, s, t) ->
      putStrLn . concat $ [prettyExpr expr, "\n-------------------\n", "Infer: ", aliasTypevar t, "\n"]
    Fail f ->
      putStrLn . concat $ [prettyExpr expr, "\n-------------------\n", "Error: ", f, "\n"]
