{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Repl (loop, initialState) where

import Adhoc (ClassEnv, initialEnv)
import Analysis (AnalyzerState)
import qualified Analysis as A
import Assump (Assump)
import Ast (ReplExp (REDecl, REExp))
import Control.Arrow (left)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Text as T
import Infer (tiExp)
import Parser (pExp, pReplExp)
import Pretty (Pretty (pretty))
import System.Console.Haskeline (InputT, getInputLine)
import TI (getSubst, runTI)
import qualified Text.Megaparsec as MP
import Types (Types (apply))

data ReplState = ReplState {ce :: ClassEnv, as :: [Assump], astate :: AnalyzerState}

type Repl a = InputT (StateT ReplState IO) a

initialState :: ReplState
initialState = ReplState initialEnv [] A.initialState

evalRepl :: ReplState -> T.Text -> Either String (ReplState, String)
evalRepl s@(ReplState ce as astate) txt = do
  re <- MP.errorBundlePretty `left` MP.runParser pReplExp "Repl" txt

  case re of
    REExp exp -> do
      (rexp, astate') <- pretty "" `left` runStateT (A.resolveExp exp) astate
      let ty = runTI $ do (_, t) <- tiExp ce as rexp; s <- getSubst; return $ apply s t

      return (s {astate = astate'}, concat [T.unpack txt, " : ", show ty])
    REDecl decl -> do
      (rdecl, astate') <- pretty "" `left` runStateT (A.resolveDecl decl) astate

      return (s {astate = astate'}, show rdecl)

loop :: Repl ()
loop = do
  s <- getInputLine "Lisy> "
  go $ maybe [] (T.splitOn " " . T.pack) s
  where
    -- TODO: add type check command
    go :: [T.Text] -> Repl ()
    go [":quit"] = return ()
    go [":q"] = return ()
    go exp = do
      st <- lift get
      case evalRepl st $ T.unwords exp of
        Left err -> liftIO $ putStrLn err
        Right (st', val) -> do
          lift $ put st'
          liftIO $ putStrLn val
      loop
