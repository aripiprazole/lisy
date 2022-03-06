module Lib
  ( someFunc,
    sample,
    std,
    toBg,
    programEnv,
  )
where

import Adhoc (ClassEnv (ClassEnv), Pred (IsIn), Qual ((:=>)), addPreludeClasses, initialEnv, (<:>))
import Assump (Assump ((:>:)))
import Ast (Alt (Alt), BindGroup (BindGroup), Exp (ELit), Expl (Expl), Impl (Impl), Lit (LString, LUnit), Pat (PVar))
import Data.Maybe (fromJust)
import Infer (tiProgram)
import Name (Name (Id))
import Reduction (simplify)
import Scheme (Scheme (Forall), toScheme)
import TI (Instantiate (inst))
import Types (Kind (KFun, KStar), TyVar (TyVar), Typ (TGen, TVar), list, tInt, tString, tUnit, (->>))

programEnv :: IO ClassEnv
programEnv = addPreludeClasses initialEnv

std :: [Assump]
std = [Id "println" :>: Forall [] ([] :=> (tString ->> tUnit))]

sample :: (Name, Maybe Scheme, [Alt])
sample = (Id "main", Just $ Forall [] ([IsIn tInt (Id "Num")] :=> (list tString ->> tUnit)), [Alt [PVar $ Id "args"] $ ELit LUnit])

toBg :: [(Name, Maybe Scheme, [Alt])] -> BindGroup
toBg g =
  BindGroup [Expl v t alts | (v, Just t, alts) <- g] $
    filter (not . null) [[Impl v alts | (v, Nothing, alts) <- g]]

someFunc :: IO ()
someFunc = do
  pe <- programEnv
  let pg = tiProgram pe std [toBg [sample]]
  let a = TVar (TyVar (Id "a") KStar)
  let b = TVar (TyVar (Id "b") KStar)

  print $ simplify pe [IsIn tInt (Id "Num")]
  print std
  print pg
  return ()
