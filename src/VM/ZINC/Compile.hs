module VM.ZINC.Compile where

-- import           Control.Monad.Reader
-- import qualified Data.Expr            as E
-- import           Data.List            (elemIndex)
-- import           VM.ZINC.Code

-- type VarList = [String]
-- type Compile a = ReaderT VarList (Either String) a

-- compileExpr :: E.Expr -> Compile ZAMCode
-- compileExpr (E.Var x) = do
--   env <- ask
--   case elemIndex x env of
--     Just i -> return [Access i]
--     _      -> lift $ Left "expr compile error"
-- compileExpr (E.Lambda x e) = do
--   c <- local ([x,"dummy"] ++) (compileExpr e)
--   return [Closure (c ++ [Return])]
-- compileExpr (E.App e1 e2) = do
--   c1 <- compileExpr e1
--   c2 <- compileExpr e2
--   return (c2 ++ c1 ++ [Apply])
-- -- compileExpr (E.Let defs body) = do
-- --   (xs,cs) <- unzip <$> compileDefs defs
-- --   let n = length xs
-- --   cBody <- local (xs ++) (compileExpr body)
-- --   return (concat cs ++ replicate n Let ++ cBody ++ replicate n EndLet)
-- compileExpr (E.LInt n) = return [Ldi n]
-- compileExpr (E.LBool b) = return [Ldb b]
-- compileExpr (E.Prim (E.Arith op e1 e2)) = do
--   c1 <- compileExpr e1
--   c2 <- compileExpr e2
--   return (c2 ++ c1 ++ [ArithBinOp op])
-- compileExpr (E.Prim (E.Comp op e1 e2)) = do
--   c1 <- compileExpr e1
--   c2 <- compileExpr e2
--   return (c2 ++ c1 ++ [CompBinOp op])
-- compileExpr (E.If e1 e2 e3) = do
--   c1 <- compileExpr e1
--   c2 <- compileExpr e2
--   c3 <- compileExpr e3
--   return (c1 ++ [Test c2 c3])

-- compileDef :: E.Def -> Compile (E.Name, ZAMCode)
-- compileDef (E.VarDef x e)   = (x,) <$> compileExpr e
-- compileDef (E.FunDef f x e) = do
--   code <- local (++ [x]) (compileExpr e)
--   return (f, [Closure (code ++ [Return])])

-- compileDefs :: [E.Def] -> Compile [(E.Name, ZAMCode)]
-- compileDefs defs = do
--   let xs = reverse [defVar d | d <- defs]
--   local (xs ++) (mapM compileDef defs)
--   where
--     defVar :: E.Def -> E.Name
--     defVar (E.VarDef x _)   = x
--     defVar (E.FunDef f _ _) = f

-- runCompile :: VarList -> E.Expr -> Either String ZAMCode
-- runCompile vars e = runReaderT (compileExpr e) vars
