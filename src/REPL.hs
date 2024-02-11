module REPL (runRepl) where

import           Control.Monad.State    (StateT, evalStateT, get, liftIO, put)
import qualified Data.Expr              as E
import           Data.List              (isPrefixOf)
import qualified Data.Map               as M
import           Data.Text.Lazy         (pack)
import           Data.Type
import qualified Interpreter.Eval       as I
import           Parser.Lang            (parseExpr, parseStmt)
import           System.Console.Repline hiding (options)
import           System.Exit
import           Typing.Infer           (TEnv, runInfer, runInferLetBinds)

data IState = IState
  { vEnv :: I.VEnv
  , tEnv :: TEnv}

initState :: IState
initState = IState mempty mempty

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err)  = liftIO (print err) >> abort

procStmt :: E.Stmt -> Repl ()
procStmt (E.ExprStmt e) = do
  ist <- get
  _ <- hoistErr $ runInfer (tEnv ist) e
  v <- hoistErr $ I.runEval (vEnv ist) e
  liftIO $ print v
procStmt (E.LetStmt isRec binds) = do
  ist <- get
  scBinds <- hoistErr $ runInferLetBinds isRec (tEnv ist) binds
  vEnv' <- hoistErr $ I.runEvalLetBinds isRec (vEnv ist) binds
  put $ ist {vEnv = vEnv', tEnv = M.union (M.fromList scBinds) (tEnv ist)}
  liftIO . putStr $ unlines [x ++ " : " ++ show sc | (x,sc) <- scBinds]

exec :: String -> Repl ()
exec source = do
  stm <- hoistErr $ parseStmt "<stdin>" (pack source)
  procStmt stm

-- :quit command
quit :: a -> Repl ()
quit _ = liftIO exitSuccess

seeType :: String -> Repl ()
seeType source = do
  ist <- get
  e <- hoistErr $ parseExpr "<stdin>" (pack source)
  t <- hoistErr $ runInfer (tEnv ist) e
  liftIO $ print t

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: Monad m => WordCompleter m
comp n = do
  let cmds = [":q", ":t"]
  return $ filter (isPrefixOf n) cmds

options :: [(String, String -> Repl ())]
options = [
    ("q"   , quit)
  , ("t", seeType)
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "REPL for minisml"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

runRepl :: IO ()
runRepl = flip evalStateT initState
     $ evalRepl (const . pure $ "minisml> ") exec options (Just ':') (Just "paste") completer ini final
