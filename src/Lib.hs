module Lib
  ( someFunc,
  )
where

import Adhoc (Pred (IsIn), Qual ((:=>)), initialEnv)
import Assump (Assump ((:>:)))
import Ast (BindGroup, Exp (ELit), Lit (LString), Pat (PVar))
import Infer (tiProgram)
import Name (Name (Id))
import Scheme (Scheme (Forall), toScheme)
import TI (Instantiate (inst))
import Types (Kind (KFun, KStar), TyVar (TyVar), Typ (TGen, TVar), tString, tUnit, (->>))

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

someFunc :: IO ()
someFunc = do
  let pg = tiProgram initialEnv [] sample
  let a = TVar (TyVar (Id "a") KStar)
  let b = TVar (TyVar (Id "b") KStar)

  print std
  print pg
  return ()
