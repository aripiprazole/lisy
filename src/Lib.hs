module Lib
  ( someFunc,
    sample,
    std,
    toBg,
    sample2,
  )
where

import Adhoc (Pred (IsIn), Qual ((:=>)), initialEnv)
import Assump (Assump ((:>:)))
import Ast (Alt, BindGroup, Exp (ELit), Lit (LString), Pat (PVar))
import Infer (tiProgram)
import Name (Name (Id))
import Scheme (Scheme (Forall), toScheme)
import TI (Instantiate (inst))
import Types (Kind (KFun, KStar), TyVar (TyVar), Typ (TGen, TVar), list, tString, tUnit, (->>))

std :: [Assump]
std = [Id "println" :>: Forall [] ([] :=> (tString ->> tUnit))]

sample :: [BindGroup]
sample =
  [ ( [ ( Id "main",
          toScheme tUnit,
          [([PVar $ Id "x"], ELit $ LString "hello")]
        )
      ],
      []
    )
  ]

sample2 :: (Name, Maybe Scheme, [Alt])
sample2 = (Id "main", Just $ Forall [] ([] :=> (list tString ->> tUnit)), [([PVar $ Id "args"], ELit $ LString "hello")])

toBg :: [(Name, Maybe Scheme, [Alt])] -> BindGroup
toBg g =
  ( [(v, t, alts) | (v, Just t, alts) <- g],
    filter (not . null) [[(v, alts) | (v, Nothing, alts) <- g]]
  )

someFunc :: IO ()
someFunc = do
  let pg = tiProgram initialEnv std sample
  let a = TVar (TyVar (Id "a") KStar)
  let b = TVar (TyVar (Id "b") KStar)

  print std
  print pg
  return ()
