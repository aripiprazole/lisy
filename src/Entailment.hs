module Entailment (bySuper, byInst, entail) where

import Adhoc (ClassEnv, Inst, Pred (IsIn), Qual ((:=>)), insts, matchPred, super)
import Control.Monad (msum)
import Types (Types (apply))

-- | Returns all predicates of class, including the super classes predicates,
-- if class N have a super class M that have a super class Y, N have Y
-- as a super class.
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn t n) = p : concat [bySuper ce (IsIn t n') | n' <- super ce n]

-- | Returns all relevant instances by an instance.
-- The `msum` function is used to prevent overlap instances.
byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn t n) = msum [tryInst inst | inst <- insts ce n]
  where
    tryInst :: Inst -> Maybe [Pred]
    tryInst (ps :=> h) = do
      s <- matchPred h p
      return $ map (apply s) ps

-- | Returns if predicate p is valid when all predicates in ps are satisfied.
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any ((p `elem`) . bySuper ce) ps || findInsts (byInst ce p)
  where
    findInsts :: Maybe [Pred] -> Bool
    findInsts Nothing = False
    findInsts (Just ps) = all (entail ce ps) ps