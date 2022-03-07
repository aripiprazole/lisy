{-# LANGUAGE OverloadedStrings #-}

module Repl (loop, initialState) where

import Control.Arrow (left)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Text as T
import Parser (pExp)
import System.Console.Haskeline (InputT, getInputLine)
import Text.Megaparsec (errorBundlePretty, runParser)

data ReplState = ReplState

type Repl a = InputT (StateT ReplState IO) a

initialState :: ReplState
initialState = ReplState

-- TODO: type check and eval
evalRepl :: T.Text -> Either String String
evalRepl txt = do
  exp <- errorBundlePretty `left` runParser pExp "Repl" txt

  return $ show exp

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
      case evalRepl $ T.unwords exp of
        Left err -> liftIO $ putStrLn err
        Right val -> liftIO $ putStrLn val
      loop
