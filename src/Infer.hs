module Infer (Infer, tiExp, tiPat, tiBindGroup, tiPats) where

import Adhoc (ClassEnv, Pred (IsIn), Qual ((:=>)))
import Assump (Assump ((:>:)), find)
import Ast (BindGroup, Exp (EApp, EConst, ELet, ELit, EVar), Lit (LChar, LInt, LRat, LString), Pat (PAs, PCon, PLit, PNpk, PVar, PWildcard))
import Name (Name (Id))
import Scheme (toScheme)
import TI (TI (TI), freshInst, newTVar, unify)
import Types (Kind (KStar), Typ, tChar, tString, (->>))

-- | Γ;P | A ⊢ e : τ, where Γ is a class environment, P is a set of predicates,
-- A is a set of assumptions, e is an expression, and τ the corresponding type.
-- Judgments like this can be thought as 5-tuple, except by using functions,
-- the input differs of the output.
type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiExp :: Infer Exp Typ
tiExp ce as (ELit l) = do
  (ps, t) <- tiLit l
  return (ps, t)
tiExp ce as (EVar n) = do
  sc <- find n as
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExp ce as (EConst (n :>: sc)) = do
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExp ce as (EApp f e) = do
  (qs, tf) <- tiExp ce as f
  (ps, te) <- tiExp ce as e
  t <- newTVar KStar
  unify (te ->> t) tf
  return (ps ++ qs, t)
tiExp ce as (ELet bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExp ce as' e
  return (ps ++ qs, t)

tiLit :: Lit -> TI ([Pred], Typ)
tiLit (LChar _) = return ([], tChar)
tiLit (LString _) = return ([], tString)
tiLit (LRat _) = do
  u <- newTVar KStar
  return ([IsIn u (Id "Fractional")], u)
tiLit (LInt _) = do
  u <- newTVar KStar
  return ([IsIn u (Id "Num")], u)

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup = undefined

tiPat :: Pat -> TI ([Pred], [Assump], Typ)
tiPat (PVar n) = do
  u <- newTVar KStar
  return ([], [n :>: toScheme u], u)
tiPat PWildcard = do
  u <- newTVar KStar
  return ([], [], u)
tiPat (PAs n pat) = do
  (ps, as, t) <- tiPat pat
  return (ps, (n :>: toScheme t) : as, t)
tiPat (PLit lit) = do
  (ps, t) <- tiLit lit
  return (ps, [], t)
tiPat (PNpk n i) = do
  u <- newTVar KStar
  return ([IsIn u (Id "Integral")], [], u)
tiPat (PCon (n :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t <- newTVar KStar
  (qs :=> t') <- freshInst sc
  unify t' (foldr (->>) t ts)

  return (ps ++ qs, as, t)

tiPats :: [Pat] -> TI ([Pred], [Assump], [Typ])
tiPats pats = do
  pats' <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- pats']
      as = concat [as' | (_, as', _) <- pats']
      ts = concat [[t] | (_, _, t) <- pats']
  return (ps, as, ts)