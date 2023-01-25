{-# LANGUAGE OverloadedStrings #-}

module HM.Pretty where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S
import HM.Expr
import HM.Infer
import HM.NameSeed
import HM.Type.TypeExpr

lookupTmap tmap k =
  case M.lookup k tmap of
    Nothing -> "?"
    Just v -> v

prettyTexpr tmap texpr =
  case texpr of
    (TypeVar (TypeVarName l)) -> B8.unpack $ lookupTmap tmap l
    (TypeCons (TIdentifier "int") _) -> "Int"
    (TypeCons (TIdentifier "bool") _) -> "Bool"
    (TypeCons (TIdentifier "arrow") [a, b]) ->
      "(" ++ prettyTexpr tmap a ++ " -> " ++ prettyTexpr tmap b ++ ")"
    (TypeCons (TIdentifier n) es) -> show n ++ " " ++ (unwords $ fmap (prettyTexpr tmap) es)

aliasTypevar :: TypeExpr -> String
aliasTypevar texpr =
  let vars = S.elems . S.fromList . map (\(TypeVarName t) -> t) $ typeVarsIn texpr
      tmap = M.fromList $ zip vars (prefixSequence startNameSeed)
   in prettyTexpr tmap texpr

prettyExpr :: Expr -> String
prettyExpr expr =
  case expr of
    Var (VarName a) -> B8.unpack a
    Lambda (VarName i) e -> "\\" ++ B8.unpack i ++ " -> " ++ prettyExpr e
    Ap e1 e2 -> prettyExpr e1 ++ " (" ++ prettyExpr e2 ++ ")"
    Let ids exprs expr' ->
      let t = zip ids exprs
          lns = map (\(VarName a, b) -> "  " ++ B8.unpack a ++ " = " ++ prettyExpr b ++ "\n") t
       in "let" ++ concat lns ++ "in " ++ prettyExpr expr'
    LetRec ids exprs expr' ->
      let t = zip ids exprs
          lns = map (\(VarName a, b) -> "\n  " ++ B8.unpack a ++ " = " ++ prettyExpr b) t
       in "let" ++ concat lns ++ "\nin " ++ prettyExpr expr'
    ELit LInt -> "Int"
    Adt (ConsName n) es -> B8.unpack n ++ " " ++ (unwords $ fmap prettyExpr es)
    Case e p es ->
      "\\case " ++ prettyExpr e ++ ": " ++ (concat $ fmap (\(a, b) -> prettyPatn a ++ " -> " ++ prettyExpr b ++ "; ") $ zip p es)
    Guard e ges es ee ->
      "\\guard " ++ prettyExpr e ++ "| " ++ (concat $ fmap (\(a,b) -> prettyExpr a ++ " = " ++ prettyExpr b ++ " | ") $ zip ges es) ++ "else = " ++ prettyExpr ee

prettyPatn p =
  case p of
    PatnVar (VarName a) -> B8.unpack a
    PatnHole -> "_"
    PatnLit lit -> show lit
    PatnCons (Cons (ConsName c) as) -> B8.unpack c ++ " " ++ (unwords $ fmap prettyPatn as)
