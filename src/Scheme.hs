module Scheme (Scheme (..), quantify, toScheme) where

import Adhoc (Qual ((:=>)))
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Types (HasKind (kind), Kind (KStar), Subst (Subst), TyVar, Typ (TGen), Types (apply, ftv), letters)

-- | Type schemes are used to describe qualified types.
-- Each TGen that appears in qt represents a generic that the kind is given
-- by `ks !! n`.
data Scheme = Forall [Kind] (Qual Typ) deriving (Eq)

instance Show Scheme where
  show (Forall [] q) = show q
  show (Forall us q) = concat ["forall ", unwords us', ". ", show q]
    where
      showKind :: Int -> Kind -> String
      showKind n KStar = "'" ++ letters !! n
      showKind n k = concat ["('", letters !! n, " : ", show k, ")"]

      us' :: [String]
      us' = toList $ S.mapWithIndex showKind $ S.fromList us

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  ftv (Forall ks qt) = ftv qt

-- | Quantify type into a Scheme. It is useful to compare two type schemes.
quantify :: [TyVar] -> Qual Typ -> Scheme
quantify us qt = Forall (kind <$> us') $ apply s qt
  where
    us' :: [TyVar]
    us' = [u | u <- ftv qt, u `elem` us]

    s :: Subst
    s = Subst $ zip us' $ TGen <$> [0 ..]

-- | Transform a type into a scheme without quantifying or adding
-- qualified predicates.
toScheme :: Typ -> Scheme
toScheme t = Forall [] ([] :=> t)