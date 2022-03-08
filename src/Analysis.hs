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
import Infer (tiBindGroup, tiImpls)
import Name (Name (Id))
import ResolvedAst (RAlt (RAlt), RBindGroup (RBindGroup), RExp (REApp, REConst, RELet, RELit, REVar), RImpl (RImpl), RPat (RPAs, RPCon, RPLit, RPNpk, RPVar, RPWildcard), RProgram (RProgram), bgFromTuples)
import Scheme (Scheme (Forall), quantify)
import TI (getSubst, runTI)
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
    go as (Var n alts Nothing) = as' ++ as
      where
        as' :: [Assump]
        as' = (snd . runTI) $ do
          a <- tiImpls ce as [RImpl n alts]
          s <- getSubst
          return $ apply s a

initialState :: AnalyzerState
initialState = AnalyzerState [] [] Nothing

setImpl :: Name -> RAlt -> Resolve ()
setImpl n a = do
  st <- get
  s <- lookupSymbol n

  when (isNothing s) $ setSymbol Nothing n

  s' <- fromJust <$> lookupSymbol n
  put st {variables = s' {alts = a : alts s'} : delete s' (variables st)}

  return ()

setSymbol :: Maybe Scheme -> Name -> Resolve ()
setSymbol sc n = do
  st <- get
  put st {variables = Var n [] sc : variables st}
  return ()

forkState :: Resolve a -> Resolve a
forkState f = do
  s <- get
  put s {enclosing = Just s}
  a <- f
  put s
  return a

lookupType :: Name -> Resolve (Maybe (Name, Kind))
lookupType n = do v <- gets types; go v
  where
    go :: [(Name, Kind)] -> Resolve (Maybe (Name, Kind))
    go [] = return Nothing
    go ((n', k) : xs)
      | n == n' = return $ Just (n', k)
      | otherwise = go xs

lookupSymbol :: Name -> Resolve (Maybe Var)
lookupSymbol n = do v <- gets variables; go v
  where
    go :: [Var] -> Resolve (Maybe Var)
    go [] = return Nothing
    go (v@(Var n' _ sc) : xs)
      | n == n' = return $ Just v
      | otherwise = go xs

generalize :: Typ -> Resolve [TyVar]
generalize = go []
  where
    go :: [TyVar] -> Typ -> Resolve [TyVar]
    go us (TVar tv@(TyVar n _)) =
      lookupType n >>= \case
        Just (_, k) -> return $ tv : us
        Nothing -> return us
    go us (TApp t1 t2) = do
      t1' <- go us t1
      t2' <- go us t2
      return $ t1' `union` t2'
    go us t = return us

resolveProgram :: Program -> Resolve RProgram
resolveProgram p = do
  error "TODO"

-- TODO: validate the existance of the given type class n
resolvePred :: Pred -> Resolve Pred
resolvePred (IsIn t n) = do
  t' <- resolveTyp t
  return $ IsIn t n

resolveScheme :: Scheme -> Resolve Scheme
resolveScheme (Forall ks (ps :=> t)) = do
  ps' <- mapM resolvePred ps
  t' <- resolveTyp t
  return $ Forall ks $ ps' :=> t'

resolveTyp :: Typ -> Resolve Typ
resolveTyp (TCon (TyCon n k)) =
  lookupType n >>= \case
    Just (n', k') -> return $ TCon $ TyCon n' k'
    Nothing -> return $ TVar $ TyVar n k
resolveTyp (TApp l r) = do
  l' <- resolveTyp l
  r' <- resolveTyp r
  return $ TApp l' r'
resolveTyp t = return t

resolveDecl :: Decl -> Resolve ()
resolveDecl (DImpl n a) = do
  a' <- forkState $ resolveAlt a
  setImpl n a'
  return ()
resolveDecl (DVal n sc) = do
  (Forall _ qt@(_ :=> t)) <- resolveScheme sc
  us <- generalize t

  setSymbol (Just $ quantify us qt) n

resolveAlt :: Alt -> Resolve RAlt
resolveAlt (Alt ps e) = do
  ps' <- sequence $ resolvePat <$> ps
  e' <- resolveExp e
  return $ RAlt ps' e'

resolvePat :: Pat -> Resolve RPat
resolvePat PWildcard = return RPWildcard
resolvePat (PLit l) = return $ RPLit l
resolvePat (PNpk n k) = return $ RPNpk n k
resolvePat (PAs n p) = do
  p' <- resolvePat p
  setSymbol Nothing n
  return $ RPAs n p'
resolvePat (PVar n) = do
  setSymbol Nothing n
  return $ RPVar n
resolvePat (PCon n ps) = do
  sc <- fmap scheme <$> lookupSymbol n
  ps' <- sequence $ resolvePat <$> ps

  case sc of
    Just (Just sc') -> return $ RPCon (n :>: sc') ps'
    Nothing | null ps' -> do setSymbol Nothing n; return $ RPVar n
    _ -> throwError $ UnresolvedVar n

resolveExp :: Exp -> Resolve RExp
resolveExp (ELit lit) = return $ RELit lit
resolveExp (EVar v) = do
  v' <- lookupSymbol v
  case v' of
    Just (Var n _ (Just sc)) -> return $ REConst (n :>: sc)
    Nothing -> throwError $ UnresolvedVar v
    _ -> return $ REVar v
resolveExp (EApp l r) = do
  l' <- resolveExp l
  r' <- resolveExp r
  return $ REApp l' r'
resolveExp (ELet vars e) = forkState $ do
  vars' <- mapM mapVar vars
  e' <- resolveExp e
  return $ RELet (bgFromTuples (map mapBg vars')) e'
  where
    mapBg :: (Name, RAlt) -> (Name, Maybe Scheme, [RAlt])
    mapBg (n, a) = (n, Nothing, [a])

    mapVar :: (Name, Alt) -> Resolve (Name, RAlt)
    mapVar (n, a) = do
      setSymbol Nothing n
      a' <- resolveAlt a
      return (n, a')
