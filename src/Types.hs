module Types (Subst, nullSubst, (+->), Types (..), (@@), merge) where

import Ast (TyVar, Typ (TApp, TCon, TGen, TVar))
import Control.Monad (msum)
import Data.List (intersect, nub, partition, union, (\\))

-- | Represents type variables's mapping to types
type Subst = [(TyVar, Typ)]

nullSubst :: Subst
nullSubst = []

(+->) :: TyVar -> Typ -> Subst
(+->) u t = [(u, t)]

class Types a where
  -- | Replaces all ocurrences of type variables in a type with the
  -- corresponding types in the given substitution
  apply :: Subst -> a -> a

  -- | Returns a set of free variables of `a`
  ftv :: a -> [TyVar]

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv = nub . concatMap ftv

instance Types Typ where
  apply s (TVar u) = case lookup u s of
    Just t -> t
    Nothing -> TVar u
  apply s (TApp a b) = TApp (apply s a) (apply s b)
  apply s t = t

  ftv (TVar u) = [u]
  ftv (TApp a b) = ftv a `union` ftv b
  ftv t = []

-- | Composes substitutions,
-- `apply (s1 @@ s2)` is equal to `apply s1 . apply s2`.
-- We can't use `++` between substitutions because bindings in s1 takes
-- precedence of any bindings in s2. For that the `merge` function is used.
-- The result of both functions preserves the types' kinds.
(@@) :: Subst -> Subst -> Subst
(@@) s1 s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

-- | Merges two substitutions, is equivalent to `s1 ++ s2` but
-- if there are any conflicts between the type variables of the two
-- substitutions, it fails, otherwise, returns `s1 ++ s2`.
-- The result preserves the types' kinds.
merge :: MonadFail m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return $ s1 ++ s2 else fail "Merge fails"
  where
    agree :: Bool
    agree = all f (map fst s1 `intersect` map fst s2)

    f :: TyVar -> Bool
    f u = apply s1 (TVar u) == apply s2 (TVar u)