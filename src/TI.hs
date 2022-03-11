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
import Control.Monad.Except (Except, MonadTrans (lift), runExcept)
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, gets)
import Scheme (Scheme (Forall))
import TIError (TIError)
import Types (Kind, Subst, TyVar (TyVar), Typ (TApp, TGen, TVar), Types (apply), enumId, nullSubst, (@@))
import Unify (mgu)

-- | Current fresh types state.
type State = Int

-- | Type inference monad.
type TI a = StateT (Subst, State) (Either TIError) a

runTI :: TI a -> Either TIError a
runTI ti = evalStateT ti (nullSubst, 0)

getSubst :: TI Subst
getSubst = gets fst

unify :: Typ -> Typ -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- lift $ mgu (apply s t1) (apply s t2)
  extSubst u

-- | Composes s with the context substituition.
extSubst :: Subst -> TI ()
extSubst s = do
  (s', n) <- get
  put (s @@ s', n)

-- | Gets a fresh TVar with kind k.
newTVar :: Kind -> TI Typ
newTVar k = do
  (s, n) <- get
  put (s, n + 1)
  return $ TVar $ TyVar (enumId n) k

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