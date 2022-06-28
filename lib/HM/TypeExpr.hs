{-# LANGUAGE OverloadedStrings #-}

module HM.TypeExpr where

import qualified Data.ByteString.Char8 as B8

--
-- TYPE EXPRESSIONS
--
data TypeExpr
  = TypeVar TypeVarName
  | TypeCons TIdentifier [TypeExpr]
  deriving (Eq)

newtype TypeVarName = TypeVarName B8.ByteString deriving (Eq, Show, Ord)

newtype TIdentifier = TIdentifier B8.ByteString deriving (Eq, Show)

typeVar :: B8.ByteString -> TypeExpr
typeVar = TypeVar . TypeVarName

-- Types
arrow t1 t2 = TypeCons (TIdentifier "arrow") [t1, t2]

int = TypeCons (TIdentifier "int") []

bool = TypeCons (TIdentifier "bool") []

instance Show TypeExpr where
  show (TypeVar (TypeVarName l)) = "a" ++ (B8.unpack l)
  show (TypeCons (TIdentifier "int") _) = "Int"
  show (TypeCons (TIdentifier "bool") _) = "Bool"
  show (TypeCons (TIdentifier "arrow") [a, b]) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show _ = "?"
