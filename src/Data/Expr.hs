module Data.Expr where

import           Data.List (intercalate)

type Name = String

data Prim =
    Arith (Int -> Int -> Int) Expr Expr
  | Comp (Int -> Int -> Bool) Expr Expr

type IsRec = Bool

data Expr =
    Var Name
  | LInt Int
  | LBool Bool
  | Prim Prim
  | If Expr Expr Expr
  | LetExpr IsRec [(Name,Expr)] Expr
  | Lambda Name Expr
  | App Expr Expr

data Stmt =
    LetStmt IsRec [(Name,Expr)]
  | ExprStmt Expr

instance Show Expr where
  show (Var x) = x
  show (LInt n) = show n
  show (LBool b) = show b
  show (Prim _) = "<<primitive>>"
  show (If e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (LetExpr isRec defs e) = "let " ++ (if isRec then " rec " else "") ++ intercalate " and " [x ++ " = " ++ show ex | (x,ex) <- defs] ++ " in " ++ show e
  show (Lambda x e) = "fn " ++ x ++ " => " ++ show e
  show (App e1 e2@Var{}) = unwords [show e1, show e2]
  show (App e1 e2@LInt{}) = unwords [show e1, show e2]
  show (App e1 e2@LBool{}) = unwords [show e1, show e2]
  show (App e1 e2) = show e1 ++ " (" ++ show e2 ++ ")"

instance Show Stmt where
  show (LetStmt isRec defs) = ("let " ++ if isRec then "rec " else "") ++ intercalate " and " [show def | def <- defs]
  show (ExprStmt e)   = show e
