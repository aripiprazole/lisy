module Reduction (inHnf, toHnf, toHnfs, simplify, reduce) where

import Adhoc (ClassEnv, Pred (IsIn))
import Data.Bool (bool)
import Entailment (byInst, entail)
import TIError (TIError (TIError))
import Types (Typ (TApp, TCon, TVar))

-- | Checks if the predicate is in the head-normal form. Class arguments
-- are required to be in the form `u t1,...,tn`, where u is a type variable,
-- and t1,...,tn are types (n >= 0).
-- The name head-normal form is used due to similarities to lambda-calculus
-- head-normal form.
inHnf :: Pred -> Bool
inHnf (IsIn t _) = isHnf' t
  where
    isHnf' :: Typ -> Bool
    isHnf' (TVar v) = True
    isHnf' (TCon v) = False
    isHnf' (TApp a _) = isHnf' a
    isHnf' _ = False

-- | Predicates that do not fit in head-normal form need to be decomposed using
-- `byInst` function.
-- toHnf :: ClassEnv -> Pred -> Either TIError [Pred]
toHnf :: ClassEnv -> Pred -> Either TIError [Pred]
toHnf ce p
  | inHnf p = pure [p]
  | otherwise = byInst ce p >>= toHnfs ce

toHnfs :: ClassEnv -> [Pred] -> Either TIError [Pred]
toHnfs ce ps = do
  ps' <- mapM (toHnf ce) ps
  pure $ concat ps'

-- | Reduces the size of predicates by removing duplicates,
-- like transforming `(Eq a, Eq a)` to `Eq a`.
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
    loop rs [] = []
    loop rs (p : ps)
      | entail ce (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps

-- | Reduces the predicates to head-normal form and simplify the list.
reduce :: ClassEnv -> [Pred] -> Either TIError [Pred]
reduce ce ps = do
  hnfs <- toHnfs ce ps
  pure $ simplify ce hnfs