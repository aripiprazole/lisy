module Name (Name (..)) where

newtype Name = Id String deriving (Eq, Ord)

instance Show Name where
  show (Id s) = s