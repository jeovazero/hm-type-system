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

runInfer expr =
  case typeCheck (TypeEnv M.empty) (startNameSeed) expr of
    Ok (_, t) ->
      putStrLn . concat $ [prettyExpr expr, "\n-------------------\n", "Infer: ", aliasTypevar t, "\n"]
    Fail f -> print f
