module Interpreter.Eval where

import           Control.Monad.Reader
import           Data.Expr
import qualified Data.Map             as M

data Val = VInt Int
  | VBool Bool
  -- 変数がある場合は関数のクロージャ
  | VClosure (Maybe Name) Expr VEnv

instance Show Val where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show VClosure{}    = "<closure>"

type VEnv = M.Map Name Val
type Eval a = ReaderT VEnv (Either String) a

unpackInt :: Expr -> Eval Int
unpackInt ex = do
  v <- eval ex
  case v of
    VInt n -> return n
    _      -> lift . Left $ "unpackInt " ++ show ex

unpackBool :: Expr -> Eval Bool
unpackBool ex = do
  v <- eval ex
  case v of
    VBool b -> return b
    _       -> lift . Left $ "unpackBool " ++ show ex

unpackFunc :: Expr -> Eval (Name, Expr, VEnv)
unpackFunc ex = do
  v <- eval ex
  case v of
    VClosure (Just x) bod clo -> return (x,bod,clo)
    _                         -> lift . Left $ "unpackFunc " ++ show ex

evalPrim :: Prim -> Eval Val
evalPrim (Arith op e1 e2) = do
  n1 <- unpackInt e1
  n2 <- unpackInt e2
  return (VInt (op n1 n2))
evalPrim (Comp op e1 e2) = do
  n1 <- unpackInt e1
  n2 <- unpackInt e2
  return (VBool (op n1 n2))

toClosure :: VEnv -> Expr -> Val
toClosure venv (Lambda x ex) = VClosure (Just x) ex venv
toClosure venv e             = VClosure Nothing e venv

eval :: Expr -> Eval Val
eval (LInt n)  = return (VInt n)
eval (LBool b) = return (VBool b)
eval (Var x) = do
  env <- ask
  case M.lookup x env of
    Nothing                       -> lift . Left $ x ++ " is not in val env"
    Just (VClosure Nothing e clo) -> local (const clo) (eval e)
    Just v                        -> return v
eval (Prim p) = evalPrim p
eval (If e1 e2 e3) = do
  b <- unpackBool e1
  if b then eval e2 else eval e3
eval (Lambda x bod) = asks $ VClosure (Just x) bod
eval (App e1 e2) = do
  (x,bod,clo) <- unpackFunc e1
  arg <- eval e2
  local (const $ M.insert x arg clo) (eval bod)
eval (LetExpr False defs bod) = do
  binds <- mapM (\(x,e) -> (x,) <$> eval e) defs
  local (M.union (M.fromList binds)) (eval bod)
eval (LetExpr True defs bod) = do
  env <- ask
  let env' = M.union (M.fromList [(x, toClosure env' e) | (x,e) <- defs]) env
  local (const env') (eval bod)

runEval :: VEnv -> Expr -> Either String Val
runEval env e = runReaderT (eval e) env

runEvalLetBinds :: IsRec -> VEnv -> [(Name,Expr)] -> Either String VEnv
runEvalLetBinds False env defs = do
  ext <- runReaderT (mapM (\(x,e) -> (x,) <$> eval e) defs) env
  return $ M.union (M.fromList ext) env
runEvalLetBinds True env defs = do
  let env' = M.union (M.fromList [(x, toClosure env' e) | (x,e) <- defs]) env
  return env'
