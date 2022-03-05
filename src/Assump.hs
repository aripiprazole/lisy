module Assump (Assump (..), find) where

import Control.Monad (MonadFail)
import Name (Name)
import Scheme (Scheme)
import Types (Types (apply, ftv))

-- | Assumption about a type variable.
data Assump = Name :>: Scheme

instance Types Assump where
  apply s (n :>: sc) = n :>: apply s sc
  ftv (_ :>: sc) = ftv sc

-- | Finds a scheme given a assumption context.
find :: MonadFail m => Name -> [Assump] -> m Scheme
find n [] = fail $ "unbound name: " ++ show n
find n ((n' :>: sc) : as)
  | n == n' = return sc
  | otherwise = find n as