module Lib
  ( someFunc,
    sample,
    std,
    toBg,
  )
where

import Adhoc (ClassEnv (ClassEnv), Pred (IsIn), Qual ((:=>)), addInst, addPreludeClasses, initialEnv, (<:>))
import Assump (Assump ((:>:)))
import Ast (Alt (Alt), BindGroup (BindGroup), Exp (EApp, ELit, EVar), Expl (Expl), Impl (Impl), Lit (LString, LUnit), Pat (PVar))
import Data.Maybe (fromJust)
import Entailment (entail)
import Infer (tiExp, tiProgram)
import Name (Name (Id))
import Reduction (simplify)
import Scheme (Scheme (Forall), toScheme)
import TI (Instantiate (inst), getSubst, runTI)
import Types (Kind (KFun, KStar), TyVar (TyVar), Typ (TGen, TVar), Types (apply), list, tInt, tString, tUnit, (->>))

atyp = TVar (TyVar (Id "a") KStar)

std :: [Assump]
std =
  [ Id "println" :>: Forall [] ([] :=> (tString ->> tUnit)),
    Id "show" :>: Forall [] ([IsIn atyp (Id "Show")] :=> (atyp ->> tString))
  ]

sample :: (Name, Maybe Scheme, [Alt])
sample =
  ( Id "main",
    Just $ Forall [] ([] :=> (list tString ->> tString)),
    [Alt [PVar $ Id "args"] $ EApp (EVar (Id "show")) (ELit LUnit)]
  )

toBg :: [(Name, Maybe Scheme, [Alt])] -> BindGroup
toBg g =
  BindGroup [Expl v t alts | (v, Just t, alts) <- g] $
    filter (not . null) [[Impl v alts | (v, Nothing, alts) <- g]]

env = [Id "args" :>: Forall [] ([] :=> list tString)]

someFunc :: IO ()
someFunc = do
  ce' <- addPreludeClasses initialEnv
  ce <- addInst [] (IsIn (list tString) (Id "Show")) ce'

  print $ tiProgram ce std [toBg [sample]]
  let (ps, t) = runTI $ do
        (qs, t) <- tiExp ce (std ++ env) (EApp (EVar $ Id "show") (EVar $ Id "args"))
        s <- getSubst

        return (apply s qs, apply s t)
  print (ps, t)
  print $ entail ce [] (head ps)
  return ()
