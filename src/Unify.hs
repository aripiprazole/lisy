module Unify where

import TIError (TIError (TIError))
import Types (HasKind (kind), Subst, TyVar, Typ (TApp, TCon, TVar), Types (ftv), merge, nullSubst, (+->), (@@))

-- | `mgu`, that stand for most geral unifier, unifies two types and returns
-- a substition s that `apply s t1` is equal to `apply s t2`, if can't unify
-- it fails
mgu :: Typ -> Typ -> Either TIError Subst
mgu (TApp a b) (TApp a' b') = do
  s1 <- mgu a a'
  s2 <- mgu b b'
  return $ s2 @@ s1
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
mgu t1 t2
  | kind t1 /= kind t2 = Left $ TIError $ concat ["kinds of types (", show t1, ": ", show $ kind t1, ") and (", show t2, ": ", show $ kind t2, ") do not unify"]
  | otherwise = Left $ TIError $ concat ["types ", show t1, " and ", show t2, " do not unify"]

-- | Unifies type variable to a type,
-- if `u` is not in free variables of `t`,
-- if kind of `u` is equal to kind of `t`, otherwise it fails.
varBind :: TyVar -> Typ -> Either TIError Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `elem` ftv t = Left $ TIError "occurs check failed"
  | kind u /= kind t = Left $ TIError "kinds do not match"
  | otherwise = return $ u +-> t

-- | Combines two types like `mgu`, but the goal is find a substitution s that
-- `apply s t1` is equal to `t2`, this operation is frequently called one-way matching.
-- It uses `merge` rather than `@@` to compose substituions.
-- If cant unify it fails.
match :: Typ -> Typ -> Either TIError Subst
match (TApp a b) (TApp a' b') = do
  s1 <- match a a'
  s2 <- match b b'
  merge s2 s1
match (TVar u) t | kind u == kind t = return $ u +-> t
match (TCon tc1) (TCon tc2) | tc1 == tc2 = return nullSubst
match _ _ = Left $ TIError "types do not match"