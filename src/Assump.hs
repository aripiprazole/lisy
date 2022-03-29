module Assump (Assump (..), find) where

import Control.Monad (MonadFail)
import Name (Name)
import Scheme (Scheme)
import TIError (TIError (TIError))
import Types (Types (apply, ftv))

-- | Assumption about a type variable.
data Assump = Name :>: Scheme deriving (Eq)

instance Show Assump where
  show (n :>: s) = show n ++ " : " ++ show s

instance Types Assump where
  apply s (n :>: sc) = n :>: apply s sc
  ftv (_ :>: sc) = ftv sc

-- | Finds a scheme given a assumption context.
find :: Name -> [Assump] -> Either TIError Scheme
find n [] = Left $ TIError $ "unbound name: " ++ show n
find n ((n' :>: sc) : as)
  | n == n' = pure sc
  | otherwise = find n as