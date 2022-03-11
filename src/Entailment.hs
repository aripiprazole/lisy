module Entailment (bySuper, byInst, entail) where

import Adhoc (ClassEnv, Inst, Pred (IsIn), Qual ((:=>)), insts, matchPred, super)
import Control.Monad (msum)
import Data.Functor ((<&>))
import TIError (TIError (TIError))
import Types (Types (apply))

-- | pures all predicates of class, including the super classes predicates,
-- if class N have a super class M that have a super class Y, N have Y
-- as a super class.
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn t n) = p : concat [bySuper ce (IsIn t n') | n' <- super ce n]

-- | pures all relevant instances by an instance.
-- The `msum` function is used to prevent overlap instances.
byInst :: ClassEnv -> Pred -> Either TIError [Pred]
byInst ce p@(IsIn t n) = sequence (tryInst <$> insts ce n) <&> msum
  where
    tryInst :: Inst -> Either TIError [Pred]
    tryInst (ps :=> h) = do
      s <- matchPred h p
      pure $ apply s <$> ps

-- | pures if predicate p is valid when all predicates in ps are satisfied.
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any ((p `elem`) . bySuper ce) ps || findInsts (byInst ce p)
  where
    findInsts :: Either TIError [Pred] -> Bool
    findInsts (Left _) = False
    findInsts (Right ps) = all (entail ce ps) ps