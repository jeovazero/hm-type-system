{-# LANGUAGE OverloadedStrings #-}

module Main where

import HM
import HM.Type.TypeExpr
import HM.Env
import Data.List
import HM.Pretty

env = 
  let
    maybeDT = dataType (tid "Maybe") [tVarName "a"]
    maybeCons = [ consType (consName "Just") [typeVar "a"]
                , consType (consName "Nothing") []
                ]

    boolDT = dataType (tid "Bool") []
    boolCons = [ consType (consName "True") []
               , consType (consName "False") []
               ]

    dts = [(maybeDT,maybeCons), (boolDT,boolCons)]
 in
    foldl' (\e (dt,cs) -> insertDataTypeEnv dt cs e) initEnv dts

boolTrue = adt (consName "True") []
boolFalse = adt (consName "False") []

maybeJust a = adt (consName "Just") [a]
maybeNothing = adt (consName "Nothing") []

run = runInferEnv env

printInfer expr = do 
  case run expr of
    Right (renv, texpr) -> do
      putStrLn .
        concat $ [ prettyExpr expr
                   , "\n-------------------\n"
                   , "Infer: "
                   , aliasTypevar texpr 
                   , "\n"
                   ]
      print . aliasTypeEnv $ typeEnv renv

    Left msg -> do
      putStrLn .
        concat $ [ prettyExpr expr
                 , "\n-------------------\n"
                 , "Error: "
                 , msg
                 , "\n"
                 ]

main :: IO ()
main = do
  printInfer boolFalse
  printInfer boolTrue
  printInfer (maybeJust boolFalse)
  printInfer maybeNothing

  printInfer $ letrec ["id"] [lam "x" (var "x")] (var "id")
  printInfer $ letrec ["fix"] [lam "f" (ap (var "f") (ap (var "fix") (var "f")))] (var "fix")
  printInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (var "id"))
  printInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") litInt)
  printInfer $ lamcase litInt [patvar (varn "x")] [var "x"]
  printInfer $
    letrec ["id"] [lam "x" (var "x")]
    (lamcase (ap (var "id") litInt) [patvar (varn "lixo")] [var "lixo"])

  printInfer $ lamcase boolFalse [patvar (varn "x"), patvar (varn "y")] [var "x", var "y"]
    
  printInfer $ letrec ["id"] [lam "x" (var "x")] (lamcase (ap (var "id") litInt) [pathole] [boolTrue])


  printInfer $ lamcase boolFalse [patvar (varn "x"), patvar (varn "y")] [var "x", litInt]

  printInfer $
    letrec
      ["id", "const"]
      [ lam "x" (var "x")
      , lam "x" (lam "y" (var "x"))]
      (lamcase
        (ap (var "id") litInt)
          [patvar (varn "lixo")] [ap (ap (var "const") boolTrue) (var "lixo")]
      )

  printInfer $ letrec ["faa", "const"] [lam "x" (var "x"),lam "x" (lam "y" (var "x"))] (ap (var "const") litInt)


  printInfer $
    lamcase
      (adt (consName "Just") [boolTrue])
      [patcons (cons (consName "Just") [patvar $ varn "x"]), pathole] [var "x", litInt]

  printInfer $ lamguard boolTrue [boolTrue, boolFalse] [litInt, litInt] litInt 
  printInfer $ lamguard boolTrue [boolTrue, boolFalse] [litInt, boolFalse] litInt 
