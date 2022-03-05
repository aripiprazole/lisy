module Adhoc where

import Ast (Name (Id), Typ, tInt)
import Data.List (union)
import Types (Subst, Types (apply, ftv))
import Unify (match, mgu)

-- | Types can be quantified by adding a list of predicates, to restrict
-- the ways in which type variables is instantiated.
--
-- Example of `Num a => a`(in haskell):
-- > [IsIn (TVar (TyVar (Id "a") KStar)) (Id "Num")] :=> TVar (TyVar (Id "a") KStar)
data Qual t = [Pred] :=> t deriving (Eq)

data Pred = IsIn Typ Name deriving (Eq)

-- | Represents a type class by a pair of lists, one containing super class names,
-- and the second containing the instance declarations.
--
-- Example of `Ord`:
-- > ([Id "Eq"], [[] :=> IsIn tInt (Id "Ord")])
--   This example captures the fact that `Eq` is a super class of `Ord`,
--   and `Ord` has a instance for the type `Int`.
type Class = ([Name], [Inst])

type Inst = Qual Pred

instance Types a => Types (Qual a) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `union` ftv t

instance Types Pred where
  apply s (IsIn t n) = IsIn (apply s t) n
  ftv (IsIn t _) = ftv t

mguPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu

matchPred :: Pred -> Pred -> Maybe Subst
matchPred = lift match

-- | Transforms unify functions to work on `Pred`.
lift :: MonadFail m => (Typ -> Typ -> m a) -> Pred -> Pred -> m a
lift f (IsIn t n) (IsIn t' n')
  | n == n' = f t t'
  | otherwise = fail "classes differ"