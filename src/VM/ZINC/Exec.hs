module VM.ZINC.Exec where

-- import qualified Data.Expr    as E
-- import           VM.ZINC.Code

-- type ExecState = (ZAMCode, ZAMStack, ZAMEnv)

-- transition :: ExecState -> ZAMValue
-- transition ([], [v], [])             = v
-- transition (Ldi n : c, st, env)      = transition (c, IntVal n : st, env)
-- transition (Ldb b : c, st, env)      = transition (c, BoolVal b : st, env)
-- transition e@(Access i : c, st, env)
--   | length env > i = transition (c, env !! i : st, env)
--   | otherwise = error ("access error" ++ show e)
-- transition (Closure c' : c, st, env) = transition (c, ClosVal c' env : st, env)
-- transition (Apply : c, ClosVal c' env' : v : st, env) =
--   transition (c', ClosVal c env : st, v : ClosVal c' env' : env')
-- transition (Return : _, v : ClosVal c' env' : st, _) =
--   transition (c', v : st, env')
-- transition (Let : c, v : st, env) = transition (c, st, v : env)
-- transition (EndLet : c, st, _ : env) = transition (c, st, env)
-- transition (Test c1 c2 : c, BoolVal b : st, env) =
--   transition ((if b then c1 else c2) ++ c, st, env)
-- transition (ArithBinOp op :c, IntVal n1 : IntVal n2 : st, env) =
--   transition (c, IntVal (op n1 n2) : st, env)
-- transition (CompBinOp op : c, IntVal n1 : IntVal n2 : st, env) =
--   transition (c, BoolVal (op n1 n2) : st, env)
-- transition e = error ("ZAM transition failed" ++ show e)
