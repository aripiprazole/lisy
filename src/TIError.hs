module TIError (TIError (..)) where

newtype TIError = TIError String
  deriving (Eq, Show)