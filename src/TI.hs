{-# LANGUAGE DeriveFunctor #-}

module TI
  ( TI (..),
    State,
    runTI,
    getSubst,
    unify,
    extSubst,
    newTVar,
    freshInst,
    Instantiate (..),
  )
where

import Adhoc (Pred (IsIn), Qual ((:=>)))
import Scheme (Scheme (Forall))
import Types (Kind, Subst, TyVar (TyVar), Typ (TApp, TGen, TVar), Types (apply), enumId, nullSubst, (@@))
import Unify (mgu)

-- | Current fresh types state.
type State = Int

-- | Type inference monad.
newtype TI a = TI (Subst -> State -> (Subst, State, a)) deriving (Functor)

instance Applicative TI where
  pure x = TI $ \s i -> (s, i, x)
  (<*>) (TI f) (TI f') = TI $ \s i ->
    let (s', i', g) = f s i
     in let (s'', i'', x) = f' s' i'
         in (s'', i'', g x)

instance Monad TI where
  (>>=) (TI f) g = TI $ \s i ->
    let (s', i', x) = f s i
     in let TI g' = g x
         in g' s' i'

instance MonadFail TI where
  fail msg = TI $ \_ _ -> error $ "type inference failed: " ++ msg

runTI :: TI a -> a
runTI (TI f) = x where (_, _, x) = f nullSubst 0

getSubst :: TI Subst
getSubst = TI $ \s i -> (s, i, s)

unify :: Typ -> Typ -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

-- | Composes s' with the context substituition.
extSubst :: Subst -> TI ()
extSubst s' = TI $ \s i -> (s' @@ s, i, ())

-- | Gets a fresh TVar with kind k.
newTVar :: Kind -> TI Typ
newTVar k = TI $ \s n -> let u = TyVar (enumId n) k in (s, n + 1, TVar u)

-- | Instantiates a scheme with new type variables of apropriated kinds.
freshInst :: Scheme -> TI (Qual Typ)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return $ inst ts qt

class Instantiate a where inst :: [Typ] -> a -> a

instance Instantiate a => Instantiate [a] where inst ts = map (inst ts)

instance Instantiate Typ where
  inst ts (TApp a b) = TApp (inst ts a) (inst ts b)
  inst ts (TGen n) = ts !! n
  inst ts t = t

instance Instantiate Pred where
  inst ts (IsIn t n) = IsIn (inst ts t) n

instance Instantiate a => Instantiate (Qual a) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t