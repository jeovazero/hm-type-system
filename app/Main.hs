{-# LANGUAGE OverloadedStrings #-}

module Main where

import HM
import HM.TypeExpr
import HM.Infer

main :: IO ()
main = do
  runInfer $ letrec ["id"] [lam "x" (var "x")] (var "id")
  runInfer $ letrec ["fix"] [lam "f" (ap (var "f") (ap (var "fix") (var "f")))] (var "fix")
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (var "id"))
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (litInt 10))
  runInfer $ lamcase (litInt 10) [patvar (varn "x")] [var "x"]
  runInfer $ lamcase (litBool False) [patvar (varn "x"), patvar (varn "y")] [var "x", var "y"]
  runInfer $ letrec ["id"] [lam "x" (var "x")] (lamcase (ap (var "id") (litInt 10)) [patvar (varn "lixo")] [var "lixo"])
  runInfer $ letrec ["id"] [lam "x" (var "x")] (lamcase (ap (var "id") (litInt 10)) [pathole] [litBool True])

  runInfer $ lamcase (litBool False) [patvar (varn "x"), patvar (varn "y")] [var "x", litInt 10]

  runInfer $
    letrec
      ["id", "const"]
      [ lam "x" (var "x")
      , lam "x" (lam "y" (var "x"))]
      (lamcase
        (ap (var "id") (litInt 10))
          [patvar (varn "lixo")] [ap (ap (var "const") (litBool True)) (var "lixo")]
      )

  runInfer $ letrec ["faa", "const"] [lam "x" (var "x"),lam "x" (lam "y" (var "x"))] (ap (var "const") (litInt 10))

  let d = dataType (tid "Maybe") [tvar "a"]
  let cs = [consType (consName "Just") [typeVar "a"]]
  let env = insertDataTypeEnv d cs initEnv 
  runInferEnv env $ (adt (consName "Just") [litInt 10])

  runInferEnv env $
    lamcase
      (adt (consName "Just") [litBool True])
      [patcons (cons (consName "Just") [patvar $ varn "x"]), pathole] [var "x", litInt 10]

  runInfer $ lamguard (litBool True) [litBool True, litBool False] [litInt 1, litInt 2] (litInt 3) 
