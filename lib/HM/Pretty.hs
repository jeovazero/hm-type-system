{-# LANGUAGE OverloadedStrings #-}

module HM.Pretty where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Set as S
import HM.Expr
import HM.Infer
import HM.NameSeed
import HM.TypeExpr

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
    _ -> "?"

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
    ELit (LInt a) -> show a
    ELit (LBool a) -> show a
    Case e p es ->
      "\\case " ++ prettyExpr e ++ ": " ++ (concat $ fmap (\(a, b) -> prettyPatn a ++ " -> " ++ prettyExpr b ++ "; ") $ zip p es)

prettyPatn p =
  case p of
    PatnVar (VarName a) -> B8.unpack a
    PatnHole -> "_"
    PatnLit lit -> show lit
