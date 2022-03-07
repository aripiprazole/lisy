{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
    sample,
    std,
  )
where

import Adhoc (ClassEnv (ClassEnv), Pred (IsIn), Qual ((:=>)), addInst, addPreludeClasses, initialEnv, (<:>))
import Assump (Assump ((:>:)))
import Ast (Alt (Alt), BindGroup (BindGroup), Decl, Exp (EApp, ELit, EVar), Expl (Expl), Impl (Impl), Lit (LString, LUnit), Pat (PVar), Program (Program))
import Data.Maybe (fromJust)
import Data.Text (Text, intercalate, pack)
import Entailment (entail)
import Infer (tiExp, tiProgram)
import Name (Name (Id))
import Parser (Parser, pAlt, pBindGroup, pExp, pTyp, parseLisy)
import Reduction (simplify)
import Scheme (Scheme (Forall), toScheme)
import TI (Instantiate (inst), getSubst, runTI)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Error (errorBundlePretty)
import Types (Kind (KFun, KStar), TyVar (TyVar), Typ (TGen, TVar), Types (apply), list, tInt, tString, tUnit, (->>))
import Unify (mgu)

atyp = TVar (TyVar (Id "a") KStar)

std :: [Assump]
std =
  [ Id "println" :>: Forall [] ([] :=> (tString ->> tUnit)),
    Id "show" :>: Forall [] ([IsIn atyp (Id "Show")] :=> (atyp ->> tString))
  ]

parseLisyIO :: Parser a -> Text -> IO a
parseLisyIO p s = case runParser p "stub" s of
  Left err -> error $ errorBundlePretty err
  Right x -> return x

sample :: Text
sample =
  intercalate
    ";"
    [ "main : (Maybe Int) -> ()\n",
      "main (Just i) = println (show 1)"
    ]

-- | Program entrypoint
-- TODO: split ast to Resolved, Typed and Base (directly from parsing).
-- TODO: do static analysis for dependency analisys,
-- and make kinds the real values.
someFunc :: IO ()
someFunc = do
  ce <-
    addPreludeClasses initialEnv
      >>= addInst [] (IsIn (list tString) (Id "Show"))

  parseLisyIO pBindGroup sample >>= print
  return ()
