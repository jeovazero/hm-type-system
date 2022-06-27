module HM.Expr where

import Data.ByteString.Char8 as B8

--
-- LANGUAGE EXPRESSIONS
--

newtype VarName = VarName B8.ByteString deriving (Eq, Show, Ord)

data Expr
  = Var VarName
  | Lambda VarName Expr
  | Ap Expr Expr
  | Let [VarName] [Expr] Expr
  | LetRec [VarName] [Expr] Expr
  | ELit Lit
  deriving (Show)

data Lit = LInt Int | LBool Bool deriving (Show, Eq)
