{-# LANGUAGE LambdaCase #-}

module Analysis (Resolve, AnalyzerState (..), RError (..), Var (..), resolveExp, resolveDecl, initialState, asFromState) where

import Adhoc (ClassEnv, Pred (IsIn), Qual ((:=>)))
import Assump (Assump ((:>:)))
import Ast (Alt (Alt), Decl (DImpl, DVal), Exp (EApp, ELet, ELit, EVar), Pat (PAs, PCon, PLit, PNpk, PVar, PWildcard), Program (Program))
import Control.Monad (void, when)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks)
import Control.Monad.State (MonadState (get, put), StateT)
import Control.Monad.State.Class (gets)
import Data.List (delete, union)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Debug.Trace (traceM)
import Infer (tiBindGroup, tiImpls)
import Name (Name (Id))
import ResolvedAst (RAlt (RAlt), RBindGroup (RBindGroup), RExp (REApp, REConst, RELet, RELit, REVar), RImpl (RImpl), RPat (RPAs, RPCon, RPLit, RPNpk, RPVar, RPWildcard), RProgram (RProgram), bgFromTuples)
import Scheme (Scheme (Forall), quantify)
import TI (getSubst, runTI)
import TIError (TIError (TIError))
import Types (Kind, TyCon (TyCon), TyVar (TyVar), Typ (TApp, TCon, TVar), Types (apply))

data Var = Var
  { name :: Name,
    alts :: [RAlt],
    scheme :: Maybe Scheme
  }
  deriving (Show, Eq)

data AnalyzerState = AnalyzerState
  { variables :: [Var],
    types :: [(Name, Kind)],
    enclosing :: Maybe AnalyzerState
  }

data RError = UnresolvedVar Name | UnresolvedType Name

type Resolve a = StateT AnalyzerState (Either RError) a

asFromState :: ClassEnv -> AnalyzerState -> [Assump]
asFromState ce (AnalyzerState [] _ _) = []
asFromState ce (AnalyzerState vars _ _) = foldl go [] vars
  where
    go :: [Assump] -> Var -> [Assump]
    go as (Var n _ (Just sc)) = n :>: sc : as
    go as (Var n alts Nothing) = as'' ++ as
      where
        as' :: Either TIError ([Pred], [Assump])
        as' = runTI $ do
          a <- tiImpls ce as [RImpl n alts]
          s <- getSubst
          pure $ apply s a

        as'' :: [Assump]
        as'' = case as' of
          Right x -> snd x
          Left _ -> []

initialState :: AnalyzerState
initialState = AnalyzerState [] [] Nothing

setImpl :: Name -> RAlt -> Resolve ()
setImpl n a = do
  st <- get
  s <- lookupSymbol n

  when (isNothing s) $ setSymbol Nothing n

  s' <- fromJust <$> lookupSymbol n
  put st {variables = s' {alts = a : alts s'} : delete s' (variables st)}

  pure ()

setSymbol :: Maybe Scheme -> Name -> Resolve ()
setSymbol sc n = do
  st <- get
  put st {variables = Var n [] sc : variables st}
  pure ()

forkState :: Resolve a -> Resolve a
forkState f = do
  s <- get
  put s {enclosing = Just s}
  a <- f
  put s
  pure a

lookupType :: Name -> Resolve (Maybe (Name, Kind))
lookupType n = do v <- gets types; go v
  where
    go :: [(Name, Kind)] -> Resolve (Maybe (Name, Kind))
    go [] = pure Nothing
    go ((n', k) : xs)
      | n == n' = pure $ Just (n', k)
      | otherwise = go xs

lookupSymbol :: Name -> Resolve (Maybe Var)
lookupSymbol n = do v <- gets variables; go v
  where
    go :: [Var] -> Resolve (Maybe Var)
    go [] = pure Nothing
    go (v@(Var n' _ sc) : xs)
      | n == n' = pure $ Just v
      | otherwise = go xs

generalize :: Typ -> Resolve [TyVar]
generalize = go []
  where
    go :: [TyVar] -> Typ -> Resolve [TyVar]
    go us (TVar tv@(TyVar n _)) =
      lookupType n >>= \case
        Just _ -> pure us
        Nothing -> pure $ tv : us
    go us t@(TApp t1 t2) = do
      t1' <- go us t1
      t2' <- go us t2
      pure $ t1' ++ t2'
    go us t = pure us

resolveProgram :: Program -> Resolve RProgram
resolveProgram p = do
  error "TODO"

-- TODO: validate the existance of the given type class n
resolvePred :: Pred -> Resolve Pred
resolvePred (IsIn t n) = do
  t' <- resolveTyp t
  pure $ IsIn t n

resolveScheme :: Scheme -> Resolve Scheme
resolveScheme (Forall ks (ps :=> t)) = do
  ps' <- mapM resolvePred ps
  t' <- resolveTyp t
  pure $ Forall ks $ ps' :=> t'

resolveTyp :: Typ -> Resolve Typ
resolveTyp (TCon (TyCon n k)) =
  lookupType n >>= \case
    Just (n', k') -> pure $ TCon $ TyCon n' k'
    Nothing -> pure $ TVar $ TyVar n k
resolveTyp (TApp l r) = do
  l' <- resolveTyp l
  r' <- resolveTyp r
  pure $ TApp l' r'
resolveTyp t = pure t

resolveDecl :: Decl -> Resolve ()
resolveDecl (DImpl n a) = do
  a' <- forkState $ resolveAlt a
  setImpl n a'
  pure ()
resolveDecl (DVal n sc) = do
  (Forall _ qt@(_ :=> t)) <- resolveScheme sc
  us <- generalize t

  setSymbol (Just $ quantify us qt) n

resolveAlt :: Alt -> Resolve RAlt
resolveAlt (Alt ps e) = do
  ps' <- sequence $ resolvePat <$> ps
  e' <- resolveExp e
  pure $ RAlt ps' e'

resolvePat :: Pat -> Resolve RPat
resolvePat PWildcard = pure RPWildcard
resolvePat (PLit l) = pure $ RPLit l
resolvePat (PNpk n k) = pure $ RPNpk n k
resolvePat (PAs n p) = do
  p' <- resolvePat p
  setSymbol Nothing n
  pure $ RPAs n p'
resolvePat (PVar n) = do
  setSymbol Nothing n
  pure $ RPVar n
resolvePat (PCon n ps) = do
  sc <- (scheme <$>) <$> lookupSymbol n
  ps' <- sequence $ resolvePat <$> ps

  case sc of
    Just (Just sc') -> pure $ RPCon (n :>: sc') ps'
    Nothing | null ps' -> do setSymbol Nothing n; pure $ RPVar n
    _ -> throwError $ UnresolvedVar n

resolveExp :: Exp -> Resolve RExp
resolveExp (ELit lit) = pure $ RELit lit
resolveExp (EVar v) = do
  v' <- lookupSymbol v
  case v' of
    Just (Var n _ (Just sc)) -> pure $ REConst (n :>: sc)
    Nothing -> throwError $ UnresolvedVar v
    _ -> pure $ REVar v
resolveExp (EApp l r) = do
  l' <- resolveExp l
  r' <- resolveExp r
  pure $ REApp l' r'
resolveExp (ELet vars e) = forkState $ do
  vars' <- mapM mapVar vars
  e' <- resolveExp e
  pure $ RELet (bgFromTuples (map mapBg vars')) e'
  where
    mapBg :: (Name, RAlt) -> (Name, Maybe Scheme, [RAlt])
    mapBg (n, a) = (n, Nothing, [a])

    mapVar :: (Name, Alt) -> Resolve (Name, RAlt)
    mapVar (n, a) = do
      setSymbol Nothing n
      a' <- resolveAlt a
      pure (n, a')
