module Lib (entrypoint) where

import Adhoc (Qual ((:=>)), addPreludeClasses, initialEnv, (<:>))
import Analysis (AnalyzerState (types, variables))
import qualified Analysis as A
import Control.Monad.Except (runExceptT)
import Control.Monad.State (StateT (runStateT))
import Name (Name (Id))
import Repl (loop, replState)
import Scheme (Scheme (Forall))
import System.Console.Haskeline (defaultSettings, runInputT)
import Types (HasKind (kind), Kind, TyCon (TyCon), Typ (TApp, TCon), tArrow, tChar, tFloat, tInt, tList, tString, tUnit, (->>))

def :: String -> Scheme -> A.Var
def n sc = A.Var (Id n) [] (Just sc)

typ :: Typ -> (Name, Kind)
typ (TCon (TyCon n k)) = (n, k)
typ _ = error "typ not supported"

astate :: A.AnalyzerState
astate =
  A.initialState
    { variables =
        [ def "print" (Forall [] ([] :=> (tString ->> tUnit)))
        ],
      types =
        [ typ tUnit,
          typ tChar,
          typ tInt,
          typ tFloat,
          typ tFloat,
          typ tList,
          typ tArrow
        ]
    }

-- | Program entrypoint
entrypoint :: IO ()
entrypoint = do
  ce <- addPreludeClasses initialEnv
  runStateT (runInputT defaultSettings loop) $ replState ce astate

  return ()
