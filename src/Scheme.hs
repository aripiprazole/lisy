module Scheme (Scheme (..), quantify, toScheme) where

import Adhoc (Qual ((:=>)))
import Ast (HasKind (kind), Kind, TyVar, Typ (TGen))
import Types (Subst, Types (apply, ftv))

-- | Type schemes are used to describe qualified types.
-- Each TGen that appears in qt represents a generic that the kind is given
-- by `ks !! n`.
data Scheme = Forall [Kind] (Qual Typ) deriving (Eq)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  ftv (Forall ks qt) = ftv qt

-- | Quantify type into a Scheme. It is useful to compare two type schemes.
quantify :: [TyVar] -> Qual Typ -> Scheme
quantify us qt = Forall ks (apply s qt)
  where
    us' :: [TyVar]
    us' = [u | u <- ftv qt, u `elem` us]

    ks :: [Kind]
    ks = map kind us'

    s :: Subst
    s = zip us' (map TGen [0 ..])

-- | Transform a type into a scheme without quantifying or adding
-- qualified predicates.
toScheme :: Typ -> Scheme
toScheme t = Forall [] ([] :=> t)