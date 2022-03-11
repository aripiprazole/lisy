module Infer (Infer, tiProgram, tiImpls, tiExpl, tiAlt, tiAlts, tiExp, tiPat, tiBindGroup, tiPats, tiSeq) where

import Adhoc (ClassEnv, Pred (IsIn), Qual ((:=>)))
import Ambiguity (defaultSubst, split)
import Assump (Assump ((:>:)), find)
import Ast (Lit (LInt, LRat, LString, LUnit))
import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError (throwError), MonadTrans (lift))
import Data.List (intersect, union, (\\))
import Debug.Trace (traceM)
import Entailment (entail)
import Name (Name (Id))
import Reduction (reduce)
import ResolvedAst (RAlt (RAlt, pats), RBindGroup (RBindGroup), RExp (REApp, REConst, RELet, RELit, REVar), RExpl (RExpl), RImpl (RImpl, iAlts, iName), RPat (RPAs, RPCon, RPLit, RPNpk, RPVar, RPWildcard), RProgram (RProgram))
import Scheme (Scheme, quantify, toScheme)
import TI (TI, applySubst, freshInst, getSubst, newTVar, runTI, unify)
import TIError (TIError (TIError))
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
  Either TIError (Subst, Subst, [Pred], [Assump])
tiProgram ce as (RProgram pg) = runTI $ do
  (ps, as') <- tiSeq tiBindGroup ce as pg
  s <- getSubst
  rs <- lift $ reduce ce $ apply s ps
  s' <- lift $ defaultSubst ce [] rs
  undefined

-- pure (s', s, rs, apply (s' @@ s) as')

tiImpls :: Infer [RImpl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (\_ -> newTVar KStar) bs
  let ns = iName <$> bs
      scs = toScheme <$> ts
      as' = zipWith (:>:) ns scs ++ as
      alts' = iAlts <$> bs
  pss <- zipWithM (tiAlts ce as) alts' ts
  s <- getSubst
  let ps' = apply s $ concat pss
      ts' = apply s ts
      fs = ftv $ apply s as
      vss = ftv <$> ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- lift $ split ce fs (foldr1 intersect vss) ps'
  if restricted bs
    then
      let gs' = gs \\ ftv rs
          scs' = quantify gs' . ([] :=>) <$> ts'
       in pure (ds ++ rs, zipWith (:>:) ns scs')
    else
      let scs' = quantify gs . (rs :=>) <$> ts'
       in pure (ds, zipWith (:>:) ns scs')

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
  (ds, rs) <- lift $ split ce fs gs ps'

  if sc /= sc'
    then throwError $ TIError "signature too general"
    else
      if not $ null rs
        then throwError $ TIError "context too weak"
        else pure ds

tiAlt :: Infer RAlt Typ
tiAlt ce as (RAlt pats e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- tiExp ce (as' ++ as) e

  pure (ps ++ qs, foldr (->>) t ts)

tiAlts :: ClassEnv -> [Assump] -> [RAlt] -> Typ -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM_ (unify t . snd) psts
  pure $ concatMap fst psts

tiExp :: Infer RExp Typ
tiExp ce as (RELit l) = do
  (ps, t) <- tiLit l
  pure (ps, t)
tiExp ce as (REVar n) = do
  sc <- lift $ find n as
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExp ce as (REConst (n :>: sc)) = do
  (ps :=> t) <- freshInst sc
  pure (ps, t)
tiExp ce as exp@(REApp f e) = do
  (qs, tf) <- tiExp ce as f
  (ps, te) <- tiExp ce as e
  t <- newTVar KStar
  unify (te ->> t) tf
  s <- getSubst
  pure (apply s $ ps ++ qs, apply s t)
tiExp ce as (RELet bg e) = do
  (ps, as') <- tiBindGroup ce as bg
  (qs, t) <- tiExp ce (as ++ as') e
  pure (ps ++ qs, t)

tiLit :: Lit -> TI ([Pred], Typ)
tiLit LUnit = pure ([], tUnit)
tiLit (LString _) = pure ([], tString)
tiLit (LRat _) = do
  u <- newTVar KStar
  pure ([IsIn u (Id "Fractional")], u)
tiLit (LInt _) = do
  u <- newTVar KStar
  pure ([IsIn u (Id "Num")], u)

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as [] = pure ([], [])
tiSeq ti ce as (bs : bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- tiSeq ti ce (as' ++ as) bss
  pure (ps ++ qs, as'' ++ as')

tiBindGroup :: Infer RBindGroup [Assump]
tiBindGroup ce as (RBindGroup es iss) = do
  let as' = [u :>: sc | (RExpl u sc _) <- es]
  (ps, as'') <- tiSeq tiImpls ce (as' ++ as) iss
  qss <- mapM (tiExpl ce (as'' ++ as' ++ as)) es
  pure (ps ++ concat qss, as'' ++ as')

tiPat :: RPat -> TI ([Pred], [Assump], Typ)
tiPat (RPVar n) = do
  u <- newTVar KStar
  pure ([], [n :>: toScheme u], u)
tiPat RPWildcard = do
  u <- newTVar KStar
  pure ([], [], u)
tiPat (RPAs n pat) = do
  (ps, as, t) <- tiPat pat
  pure (ps, (n :>: toScheme t) : as, t)
tiPat (RPLit lit) = do
  (ps, t) <- tiLit lit
  pure (ps, [], t)
tiPat (RPNpk n i) = do
  u <- newTVar KStar
  pure ([IsIn u (Id "Integral")], [], u)
tiPat (RPCon (n :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t <- newTVar KStar
  (qs :=> t') <- freshInst sc
  unify t' (foldr (->>) t ts)

  pure (ps ++ qs, as, t)

tiPats :: [RPat] -> TI ([Pred], [Assump], [Typ])
tiPats pats = do
  pats' <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- pats']
      as = concat [as' | (_, as', _) <- pats']
      ts = [t | (_, _, t) <- pats']
  pure (ps, as, ts)

-- | A set of impl is restricted when a impl is simple, being simple is
-- when it has an alternative with no left-hand patterns.
restricted :: [RImpl] -> Bool
restricted = any simple
  where
    simple :: RImpl -> Bool
    simple (RImpl _ alts) = any (null . pats) alts