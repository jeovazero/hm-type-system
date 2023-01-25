{-# LANGUAGE OverloadedStrings #-}

module Main where

import HM
import HM.Type.TypeExpr
import HM.Env
import HM.Infer
import Data.List

env = 
  let
    maybeDT = dataType (tid "Maybe") [tvar "a"]
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

main :: IO ()
main = do
  run boolFalse
  run boolTrue
  run (maybeJust boolFalse)
  run maybeNothing

  runInfer $ letrec ["id"] [lam "x" (var "x")] (var "id")
  runInfer $ letrec ["fix"] [lam "f" (ap (var "f") (ap (var "fix") (var "f")))] (var "fix")
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (var "id"))
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") litInt)
  runInfer $ lamcase litInt [patvar (varn "x")] [var "x"]
  runInfer $
    letrec ["id"] [lam "x" (var "x")]
    (lamcase (ap (var "id") litInt) [patvar (varn "lixo")] [var "lixo"])

  run $ lamcase boolFalse [patvar (varn "x"), patvar (varn "y")] [var "x", var "y"]
    
  run $ letrec ["id"] [lam "x" (var "x")] (lamcase (ap (var "id") litInt) [pathole] [boolTrue])


  run $ lamcase boolFalse [patvar (varn "x"), patvar (varn "y")] [var "x", litInt]

  run $
    letrec
      ["id", "const"]
      [ lam "x" (var "x")
      , lam "x" (lam "y" (var "x"))]
      (lamcase
        (ap (var "id") litInt)
          [patvar (varn "lixo")] [ap (ap (var "const") boolTrue) (var "lixo")]
      )

  runInfer $ letrec ["faa", "const"] [lam "x" (var "x"),lam "x" (lam "y" (var "x"))] (ap (var "const") litInt)


  run $
    lamcase
      (adt (consName "Just") [boolTrue])
      [patcons (cons (consName "Just") [patvar $ varn "x"]), pathole] [var "x", litInt]

  run $ lamguard boolTrue [boolTrue, boolFalse] [litInt, litInt] litInt 
  run $ lamguard boolTrue [boolTrue, boolFalse] [litInt, boolFalse] litInt 
