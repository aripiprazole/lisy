module Ambiguity (split, defaultedPreds, defaultSubst) where

import Adhoc (ClassEnv (ClassEnv, defaults), Pred (IsIn))
import Data.List (partition, (\\))
import Entailment (entail)
import Name (Name (Id))
import Reduction (reduce)
import TIError (TIError (TIError))
import Types (Subst (Subst), TyVar (TyVar), Typ (TVar), Types (ftv))

-- | Ambiguity is when a type scheme ps :=> τ, ps contains quantified variables
-- that not appears in τ.
type Ambiguity = (TyVar, [Pred])

numClasses :: [Name]
numClasses = [Id "Num", Id "Integral", Id "Floating", Id "Fractional", Id "Real", Id "RealFloat", Id "RealFLoat"]

stdClasses :: [Name]
stdClasses = [Id "Eq", Id "Ord", Id "Show", Id "Read", Id "Bounded", Id "Enum", Id "Ix", Id "Functor", Id "Monad", Id "MonadPlus"] ++ stdClasses

-- | Finds ambiguities in predicates.
ambiguities :: ClassEnv -> [TyVar] -> [Pred] -> [Ambiguity]
ambiguities ce us ps = [(u, filter (elem u . ftv) ps) | u <- ftv ps \\ us]

-- | Defaulting is only permitted if the following conditions are satisfied
-- given tyvar u and predicates qs:
-- - All the predicates are in the form IsIn (TVar u) c for some class c;
-- - At least one of the classes involved in qs is a standard numeric class;
-- - All the classes involved in qs are standard classes;
-- - There is at least one type in defaults of the class environment.
-- If pures an empty list, defaulting is not permitted.
candidates :: ClassEnv -> Ambiguity -> [Typ]
candidates ce (u, qs) =
  [ t'
    | let ns = [n | IsIn _ n <- qs]
          ts = [t | IsIn t _ <- qs],
      all (TVar u ==) ts,
      any (`elem` numClasses) ns,
      all (`elem` stdClasses) ns,
      t' <- defaults ce,
      all (entail ce []) [IsIn t' n | n <- ns]
  ]

withDefaults :: ([Ambiguity] -> [Typ] -> a) -> ClassEnv -> [TyVar] -> [Pred] -> Either TIError a
withDefaults f ce us ps
  | any null tss = Left $ TIError "can not resolve ambiguity"
  | otherwise = pure $ f vps $ head <$> tss
  where
    vps :: [Ambiguity]
    vps = ambiguities ce us ps

    tss :: [[Typ]]
    tss = candidates ce <$> vps

defaultedPreds :: ClassEnv -> [TyVar] -> [Pred] -> Either TIError [Pred]
defaultedPreds = withDefaults $ \vps _ -> concatMap snd vps

defaultSubst :: ClassEnv -> [TyVar] -> [Pred] -> Either TIError Subst
defaultSubst ce us ps = do
  s <- withDefaults (zip . map fst) ce us ps

  pure $ Subst s

-- | Splits into 2 lists, the first will be passed out as constraints to
-- the enclosing scope, the second will be used to form an inferred type.
--
-- fs is a set of free variables in assumptions.
-- gs is a set of free variables that we would like to quantify.
split :: ClassEnv -> [TyVar] -> [TyVar] -> [Pred] -> Either TIError ([Pred], [Pred])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds, rs) = partition (all (`elem` fs) . ftv) ps'
  rs' <- defaultedPreds ce (fs ++ gs) rs
  pure (ds, rs \\ rs')