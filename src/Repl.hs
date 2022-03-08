{-# LANGUAGE OverloadedStrings #-}

module Repl (replState, loop) where

import Adhoc (ClassEnv, Qual ((:=>)), initialEnv)
import Analysis (AnalyzerState (types, variables))
import qualified Analysis as A
import Assump (Assump, find)
import Ast (ReplExp (REDecl, REExp))
import Control.Arrow (left)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Text as T
import Debug.Trace (traceM)
import Infer (tiExp)
import Name (Name (Id))
import Parser (pExp, pReplExp)
import Pretty (Pretty (pretty))
import Scheme (Scheme (Forall))
import System.Console.Haskeline (InputT, getInputLine)
import TI (getSubst, runTI)
import qualified Text.Megaparsec as MP
import Types (Types (apply))

data ReplState = ReplState {ce :: ClassEnv, as :: [Assump], astate :: AnalyzerState}

type Repl a = InputT (StateT ReplState IO) a

replState :: ClassEnv -> AnalyzerState -> ReplState
replState ce as = ReplState ce (A.asFromState ce as) as

classEnv :: ClassEnv
classEnv = initialEnv

evalRepl :: ReplState -> T.Text -> Either String (ReplState, String)
evalRepl s@(ReplState ce as astate) txt = do
  re <- MP.errorBundlePretty `left` MP.runParser pReplExp "Repl" txt

  case re of
    REExp exp -> do
      (rexp, astate') <- pretty "" `left` runStateT (A.resolveExp exp) astate
      let as' = A.asFromState ce astate'
          as'' = as' ++ as
          (s', (ps, t)) = runTI $ do e <- tiExp ce as'' rexp; s <- getSubst; return (s, apply s e)
          res = case ps of
            [] -> concat [T.unpack txt, " : ", show $ apply s' t]
            _ -> concat [T.unpack txt, " : (", unwords $ map show $ apply s' ps, ") => ", show $ apply s' t]

      return (s {astate = astate', as = as''}, res)
    REDecl decl -> do
      (rdecl, astate') <- pretty "" `left` runStateT (A.resolveDecl decl) astate

      let as' = A.asFromState ce astate'

      return (s {astate = astate', as = as' ++ as}, show rdecl)

loop :: Repl ()
loop = do
  s <- getInputLine "Lisy> "
  go $ maybe [] (T.splitOn " " . T.pack) s
  where
    -- TODO: add type check command
    go :: [T.Text] -> Repl ()
    go [":quit"] = return ()
    go [":q"] = return ()
    go [":type", n] = do
      st <- lift get
      sc <- find (Id (T.unpack n)) $ as st
      liftIO $ putStrLn $ T.unpack n ++ " : " ++ show sc
      loop
    go [":as"] = do
      st <- lift get
      liftIO $ print $ as st
      loop
    go exp = do
      st <- lift get
      case evalRepl st $ T.unwords exp of
        Left err -> liftIO $ putStrLn err
        Right (st', val) -> do
          lift $ put st'
          liftIO $ putStrLn val
      loop
