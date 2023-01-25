{-# LANGUAGE OverloadedStrings #-}

module HM.Type.TypeExpr where

import qualified Data.ByteString.Char8 as B8
import Data.List

--
-- TYPE EXPRESSIONS
--
data TypeExpr
  = TypeVar TypeVarName
  | TypeCons TIdentifier [TypeExpr]
  deriving (Eq)

newtype TypeVarName = TypeVarName B8.ByteString deriving (Eq, Show, Ord)

newtype TIdentifier = TIdentifier B8.ByteString deriving (Eq, Show, Ord)

typeVar :: B8.ByteString -> TypeExpr
typeVar = TypeVar . TypeVarName

-- Types
arrow t1 t2 = TypeCons (TIdentifier "arrow") [t1, t2]

int = TypeCons (TIdentifier "int") []

bool = TypeCons (TIdentifier "bool") []

instance Show TypeExpr where
  show (TypeVar (TypeVarName l)) = "a" ++ (B8.unpack l)
  show (TypeCons (TIdentifier "arrow") [a, b]) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TypeCons (TIdentifier n) _) = show n 

typeVarsIn :: TypeExpr -> [TypeVarName]
typeVarsIn texpr = typeVarsIn' texpr []
 where
  typeVarsIn' (TypeVar tname) acc = tname : acc
  typeVarsIn' (TypeCons _ ts) acc = foldl' (flip typeVarsIn') acc ts

