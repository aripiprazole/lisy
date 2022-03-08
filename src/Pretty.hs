module Pretty (Pretty (..)) where

import Analysis (RError (UnresolvedType, UnresolvedVar))

type Indent = String

class Pretty a where
  pretty :: Indent -> a -> String

instance Pretty RError where
  pretty _ (UnresolvedVar n) = "Unresolved variable: " ++ show n
  pretty _ (UnresolvedType n) = "Unresolved type: " ++ show n