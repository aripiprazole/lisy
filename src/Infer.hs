module Infer (Infer, tiProgram, tiImpls, tiExpl, tiAlt, tiAlts, tiExp, tiPat, tiBindGroup, tiPats, tiSeq) where

import Adhoc (ClassEnv, Pred (IsIn), Qual ((:=>)))
import Ambiguity (defaultSubst, split)
import Assump (Assump ((:>:)), find)
import Ast (Lit (LInt, LRat, LString, LUnit))
import Control.Monad (zipWithM)
import Data.List (intersect, union, (\\))
import Entailment (entail)
import Name (Name (Id))
import Reduction (reduce)
import ResolvedAst (RAlt (RAlt, pats), RBindGroup (RBindGroup), RExp (REApp, REConst, RELet, RELit, REVar), RExpl (RExpl), RImpl (RImpl, iAlts, iName), RPat (RPAs, RPCon, RPLit, RPNpk, RPVar, RPWildcard), RProgram (RProgram))
import Scheme (Scheme, quantify, toScheme)
import TI (TI (TI), freshInst, getSubst, newTVar, runTI, unify)
import Types (Kind (KStar), Subst (Subst), Typ, Types (apply, ftv), tChar, tString, tUnit, (->>), (@@))

-- | Γ;P | A ⊢ e : τ, where Γ is a class environment, P is a set of predicates,
-- A is a set of assumptions, e is an expression, and τ the corresponding type.
-- Judgments like this can be thought as 5-tuple, except by using functions,
-- the input differs of the output.
type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiProgram ::
  ClassEnv ->
  [Assump] ->
  RProgram ->
  (Subst, Subst, [Pred], [Assump])
tiProgram ce as (RProgram pg) = runTI $ do
  (ps, as') <- tiSeq tiBindGroup ce as pg
  s <- getSubst
  rs <- reduce ce $ apply s ps
  s' <- defaultSubst ce [] rs
  return (s', s, rs, apply (s' @@ s) as')

tiImpls :: Infer [RImpl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (\_ -> newTVar KStar) bs
  let ns = map iName bs
      scs = map toScheme ts
      as' = zipWith (:>:) ns scs ++ as
      alts' = map iAlts bs
  pss <- zipWithM (tiAlts ce as) alts' ts
  s <- getSubst
  let ps' = apply s $ concat pss
      ts' = apply s ts
      fs = ftv $ apply s as
      vss = map ftv ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restricted bs
    then
      let gs' = gs \\ ftv rs
          scs' = map (quantify gs' . ([] :=>)) ts'
       in return (ds ++ rs, zipWith (:>:) ns scs')
    else
      let scs' = map (quantify gs . (rs :=>)) ts'
       in return (ds, zipWith (:>:) ns scs')

tiExpl :: ClassEnv -> [Assump] -> RExpl -> TI [Pred]
tiExpl ce as (RExpl n sc alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- getSubst
  let qs' = apply s qs
      t' = apply s t
      fs = ftv (apply s as)
      gs = ftv t' \\ fs
      sc' = quantify gs (qs' :=> t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds, rs) <- split ce fs gs ps'

  if sc /= sc'
    then fail "signature too general"
    else
      if not $ null rs
        then fail "context too weak"
        else return ds

tiAlt :: Infer RAlt Typ
tiAlt ce as (RAlt pats e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExp ce (as' ++ as) e

  return (ps ++ qs, foldr (->>) t ts)

tiAlts :: ClassEnv -> [Assump] -> [RAlt] -> Typ -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t . snd) psts
  return $ concatMap fst psts

tiExp :: Infer RExp Typ
tiExp ce as (RELit l) = do
  (ps, t) <- tiLit l
  return (ps, t)
tiExp ce as (REVar n) = do
  sc <- find n as
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExp ce as (REConst (n :>: sc)) = do
  (ps :=> t) <- freshInst sc
  return (ps, t)
tiExp ce as (REApp f e) = do
  (qs, tf) <- tiExp ce as f
  (ps, te) <- tiExp ce as e
  t <- newTVar KStar
  unify (te ->> t) tf
  return (ps ++ qs, t)
tiExp ce as (RELet bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExp ce (as ++ as') e
  return (ps ++ qs, t)

tiLit :: Lit -> TI ([Pred], Typ)
tiLit LUnit = return ([], tUnit)
tiLit (LString _) = return ([], tString)
tiLit (LRat _) = do
  u <- newTVar KStar
  return ([IsIn u (Id "Fractional")], u)
tiLit (LInt _) = do
  u <- newTVar KStar
  return ([IsIn u (Id "Num")], u)

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as [] = return ([], [])
tiSeq ti ce as (bs : bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

tiBindGroup :: Infer RBindGroup [Assump]
tiBindGroup ce as (RBindGroup es iss) = do
  let as' = [u :>: sc | (RExpl u sc _) <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
  return (ps ++ concat qss, as'' ++ as')

tiPat :: RPat -> TI ([Pred], [Assump], Typ)
tiPat (RPVar n) = do
  u <- newTVar KStar
  return ([], [n :>: toScheme u], u)
tiPat RPWildcard = do
  u <- newTVar KStar
  return ([], [], u)
tiPat (RPAs n pat) = do
  (ps, as, t) <- tiPat pat
  return (ps, (n :>: toScheme t) : as, t)
tiPat (RPLit lit) = do
  (ps, t) <- tiLit lit
  return (ps, [], t)
tiPat (RPNpk n i) = do
  u <- newTVar KStar
  return ([IsIn u (Id "Integral")], [], u)
tiPat (RPCon (n :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t <- newTVar KStar
  (qs :=> t') <- freshInst sc
  unify t' (foldr (->>) t ts)

  return (ps ++ qs, as, t)

tiPats :: [RPat] -> TI ([Pred], [Assump], [Typ])
tiPats pats = do
  pats' <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- pats']
      as = concat [as' | (_, as', _) <- pats']
      ts = concat [[t] | (_, _, t) <- pats']
  return (ps, as, ts)

-- | A set of impl is restricted when a impl is simple, being simple is
-- when it has an alternative with no left-hand patterns.
restricted :: [RImpl] -> Bool
restricted = any simple
  where
    simple :: RImpl -> Bool
    simple (RImpl _ alts) = any (null . pats) alts