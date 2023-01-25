module HM.Expr where

import Data.ByteString.Char8 as B8

--
-- LANGUAGE EXPRESSIONS
--

newtype VarName = VarName B8.ByteString deriving (Eq, Show, Ord)
newtype ConsName = ConsName B8.ByteString deriving (Eq, Show, Ord)

data Expr
  = Var VarName
  | Lambda VarName Expr
  | Ap Expr Expr
  | Let [VarName] [Expr] Expr
  | LetRec [VarName] [Expr] Expr
  | Case Expr [Patn] [Expr]
  | Guard Expr [Expr] [Expr] Expr
  | ELit Lit
  | Adt ConsName [Expr]
  deriving (Show)

data Patn = PatnHole | PatnLit Lit | PatnVar VarName | PatnCons Cons deriving (Show, Eq)

data Lit = LInt deriving (Show, Eq)

data Cons = Cons ConsName [Patn] deriving (Show, Eq)
