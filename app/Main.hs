{-# LANGUAGE OverloadedStrings #-}

module Main where

import HM

main :: IO ()
main = do
  runInfer $ letrec ["id"] [lam "x" (var "x")] (var "id")
  runInfer $ letrec ["fix"] [lam "f" (ap (var "f") (ap (var "fix") (var "f")))] (var "fix")
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (var "id"))
  runInfer $ letrec ["id"] [lam "x" (var "x")] (ap (var "id") (litInt 10))
