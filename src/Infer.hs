module Infer (Infer, tiProgram, tiImpls, tiExpl, tiAlt, tiAlts, tiExp, tiPat, tiBindGroup, tiPats, tiSeq) where

import Adhoc (ClassEnv, Pred (IsIn), Qual ((:=>)))
import Ambiguity (defaultSubst, split)
import Assump (Assump ((:>:)), find)
import Ast (Alt (Alt, pats), BindGroup (BindGroup), Exp (EApp, EConst, ELet, ELit, EVar), Expl (Expl), Impl (Impl, iAlts, iName), Lit (LChar, LInt, LRat, LString, LUnit), Pat (PAs, PCon, PLit, PNpk, PVar, PWildcard), Program)
import Control.Monad (zipWithM)
import Data.List (intersect, union, (\\))
import Entailment (entail)
import Name (Name (Id))
import Reduction (reduce)
import Scheme (Scheme, quantify, toScheme)
import TI (TI (TI), freshInst, getSubst, newTVar, runTI, unify)
import Types (Kind (KStar), Typ, Types (apply, ftv), tChar, tString, tUnit, (->>), (@@))

-- | Γ;P | A ⊢ e : τ, where Γ is a class environment, P is a set of predicates,
-- A is a set of assumptions, e is an expression, and τ the corresponding type.
-- Judgments like this can be thought as 5-tuple, except by using functions,
-- the input differs of the output.
type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

tiProgram ce as pg = runTI $ do
  (ps, as') <- tiSeq tiBindGroup ce as pg
  s <- getSubst
  rs <- reduce ce $ apply s ps
  s' <- defaultSubst ce [] rs
  return (s', s, rs, apply (s' @@ s) as')

tiImpls :: Infer [Impl] [Assump]
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

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (Expl n sc alts) = do
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

tiAlt :: Infer Alt Typ
tiAlt ce as (Alt pats e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExp ce (as' ++ as) e

  return (ps ++ qs, foldr (->>) t ts)

tiAlts :: ClassEnv -> [Assump] -> [Alt] -> Typ -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t . snd) psts
  return $ concatMap fst psts

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
tiLit LUnit = return ([], tUnit)
tiLit (LChar _) = return ([], tChar)
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

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (BindGroup es iss) = do
  let as' = [u :>: sc | (Expl u sc _) <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
  return (ps ++ concat qss, as'' ++ as')

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

-- | A set of impl is restricted when a impl is simple, being simple is
-- when it has an alternative with no left-hand patterns.
restricted :: [Impl] -> Bool
restricted = any simple
  where
    simple :: Impl -> Bool
    simple (Impl _ alts) = any (null . pats) alts