module Typing.Infer where

import           Control.Monad.RWS
import           Data.Expr
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Data.Type
import           Typing.Subst

type TEnv = M.Map Name Scheme
type Infer a = RWST TEnv () Int (Either String) a

unify :: Type -> Type -> Infer Subst
unify t1 t2 = case solve [(t1,t2)] mempty of
  Just sub -> return sub
  _        -> lift . Left $ "unification fail: " ++ show t1 ++ " " ++ show t2

typeVars :: [String]
typeVars = [x | n <- [1..], x <- replicateM n ['a'..'z']]

freshVar :: Infer Type
freshVar = do
  i <- get
  modify succ
  return $ TVar (typeVars !! i)

instanciate :: Scheme -> Infer Type
instanciate (Forall xs t) = do
  let tvs = S.toList xs
  tvs' <- mapM (const freshVar) tvs
  let sub = M.fromList (zip tvs tvs')
  return (apply sub t)

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  let xs = S.difference (ftv t) (ftv env)
  return (Forall xs t)

inEnv :: [(Name,Scheme)] -> Infer a -> Infer a
inEnv bs = local scope
  where
    scope :: TEnv -> TEnv
    scope env = foldr (\(x,sc) e -> M.insert x sc e) env bs

lookupTEnv :: Name -> Infer Type
lookupTEnv x = do
  env <- ask
  case M.lookup x env of
    Nothing -> lift . Left $ "variable not found: " ++ x
    Just sc -> instanciate sc

--- inference processes

inferFunc :: [Expr] -> Type -> Infer (Type, Subst)
inferFunc args expected = do
  tv <- freshVar
  (tf, sub1) <- foldM inferStep (id, mempty) args
  sub2 <- unify (apply sub1 (tf tv)) expected
  return (apply sub2 tv, sub2 `compose` sub1)
  where
    inferStep :: (Type -> Type, Subst) -> Expr -> Infer (Type -> Type, Subst)
    inferStep (tf,sub) e = do
      (t,sub') <- local (apply sub) (infer e)
      return (tf . TArrow t, sub' `compose` sub)

inferPrim :: Prim -> Infer (Type, Subst)
inferPrim (Arith _ e1 e2) = inferFunc [e1,e2] $ TInt `TArrow` (TInt `TArrow` TInt)
inferPrim (Comp _ e1 e2) = inferFunc [e1,e2] $ TInt `TArrow` (TInt `TArrow` TBool)

inferBind :: ([Scheme], Subst) -> Expr -> Infer ([Scheme], Subst)
inferBind (scs,sub) e = do
  (t,sub') <- local (apply sub) (infer e)
  sc <- generalize t
  return (scs ++ [sc], sub' `compose` sub)

inferBinds :: [(Name,Expr)] -> Infer ([(Name,Scheme)], Subst)
inferBinds binds = do
  let (xs,es) = unzip binds
  (scs,sub) <- foldM inferBind mempty es
  return (zip xs (apply sub scs), sub)

inferRecBinds :: [(Name,Expr)] -> Infer ([(Name,Scheme)], Subst)
inferRecBinds binds = do
  let (xs,es) = unzip binds
  tvs <- replicateM (length binds) freshVar
  let xBinds = [(x, Forall mempty tv) | (x,tv) <- zip xs tvs]
  (_,sub) <- inEnv xBinds (foldM inferBind mempty es)
  return ([(x, apply sub t) | (x,t) <- xBinds], sub)

infer :: Expr -> Infer (Type, Subst)
infer LInt{}           = return (TInt, mempty)
infer LBool{}          = return (TBool, mempty)
infer (Var x) = (,) <$> lookupTEnv x <*> return mempty
infer (Prim pr) = inferPrim pr
infer (If e1 e2 e3) = do
  tv <- freshVar
  inferFunc [e1,e2,e3] (TBool `TArrow` (tv `TArrow` (tv `TArrow` tv)))
infer (Lambda x e) = do
  tv <- freshVar
  (tBody,sub) <- local (M.insert x (Forall mempty tv)) (infer e)
  return (apply sub tv `TArrow` tBody, sub)
infer (App e1 e2) = do
  tv <- freshVar
  (t1,sub1) <- infer e1
  (t2,sub2) <- local (apply sub1) (infer e2)
  sub3 <- unify (apply sub2 t1) (t2 `TArrow` tv)
  return (apply sub3 tv, sub3 `compose` sub2 `compose` sub1)
infer (LetExpr isRec binds body) = do
  (bs,sub) <- (if isRec then inferRecBinds else inferBinds) binds
  let (xs,scs) = unzip bs
  inEnv (zip xs (apply sub scs)) (infer body)

runInferLetBinds :: IsRec -> TEnv -> [(Name,Expr)] -> Either String [(Name,Scheme)]
runInferLetBinds isRec env binds = do
  let inferM = if isRec then inferRecBinds binds else inferBinds binds
  ((sBinds,_),_, _) <- runRWST inferM env 0
  return sBinds

runInfer :: TEnv -> Expr -> Either String Type
runInfer env expr = case runRWST (infer expr) env 0 of
  Right ((t,_),_, _) -> return t
  Left err           -> Left err
