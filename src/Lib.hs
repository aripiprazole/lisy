module Lib (entrypoint) where

import Control.Monad.Except (runExceptT)
import Control.Monad.State (StateT (runStateT))
import Repl (initialState, loop)
import System.Console.Haskeline (defaultSettings, runInputT)

-- | Program entrypoint
-- TODO: do static analysis for dependency analisys,
-- and make kinds the real values.
entrypoint :: IO ()
entrypoint = do
  runStateT (runInputT defaultSettings loop) initialState

  return ()
