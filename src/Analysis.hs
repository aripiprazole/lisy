{-# LANGUAGE LambdaCase #-}

module Analysis (Resolve, AnalyzerState (..), resolveExp, resolveDecl, initialState, prettyRError) where

import Adhoc (Pred (IsIn), Qual ((:=>)))
import Assump (Assump ((:>:)))
import Ast (Alt (Alt), Decl (DImpl, DVal), Exp (EApp, ELet, ELit, EVar), Pat (PAs, PCon, PLit, PNpk, PVar, PWildcard), Program (Program))
import Control.Monad (void)
import Control.Monad.Except (Except, MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), ReaderT, asks)
import Control.Monad.State (MonadState (get, put), StateT)
import Control.Monad.State.Class (gets)
import Data.Maybe (mapMaybe)
import Name (Name (Id))
import ResolvedAst (RAlt (RAlt), RBindGroup (RBindGroup), RExp (REApp, REConst, RELet, RELit, REVar), RPat (RPAs, RPCon, RPLit, RPNpk, RPVar, RPWildcard), RProgram (RProgram), bgFromTuples)
import Scheme (Scheme (Forall))
import Types (Kind, TyCon (TyCon), TyVar (TyVar), Typ (TApp, TCon, TVar))

data AnalyzerState = AnalyzerState
  { variables :: [(Name, Maybe Scheme)],
    types :: [(Name, Kind)],
    impls :: [(Name, Alt)],
    enclosing :: Maybe AnalyzerState
  }

data RError = UnresolvedVar Name | UnresolvedType Name

type Resolve a = StateT AnalyzerState (Either RError) a

asFromState :: AnalyzerState -> [Assump]
asFromState (AnalyzerState vars _ _ _) = mapMaybe mapVar vars
  where
    mapVar :: (Name, Maybe Scheme) -> Maybe Assump
    mapVar (n, Just sc) = Just $ n :>: sc
    mapVar (_, Nothing) = Nothing

prettyRError :: RError -> String
prettyRError (UnresolvedVar n) = "Unresolved variable: " ++ show n
prettyRError (UnresolvedType n) = "Unresolved type: " ++ show n

initialState :: AnalyzerState
initialState = AnalyzerState [] [] [] Nothing

setImpl :: Name -> Alt -> Resolve ()
setImpl n a = do
  s <- get
  put s {impls = (n, a) : impls s}
  return ()

setSymbol :: Maybe Scheme -> Name -> Resolve ()
setSymbol sc n = do
  s <- get
  put s {variables = (n, sc) : variables s}
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

lookupSymbol :: Name -> Resolve (Maybe (Name, Maybe Scheme))
lookupSymbol n = do v <- gets variables; go v
  where
    go :: [(Name, Maybe Scheme)] -> Resolve (Maybe (Name, Maybe Scheme))
    go [] = return Nothing
    go ((n', sc) : xs)
      | n == n' = return $ Just (n', sc)
      | otherwise = go xs

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
  setImpl n a
  forkState $ do
    a' <- resolveAlt a
    return ()
resolveDecl (DVal n sc) = setSymbol (Just sc) n

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
  sc <- fmap snd <$> lookupSymbol n
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
    Just (n, Just sc) -> return $ REConst (n :>: sc)
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
