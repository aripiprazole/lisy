{-# LANGUAGE RankNTypes #-}

module Adhoc
  ( Qual (..),
    Pred (..),
    Class,
    Inst,
    mguPred,
    matchPred,
    ClassEnv (..),
    EnvTransformer,
    (<:>),
    super,
    insts,
    initialEnv,
    modify,
    addClass,
    addInst,
    addPreludeClasses,
  )
where

import Data.Either (isRight)
import Data.List (intercalate, union)
import Data.Maybe (isJust, isNothing)
import Name (Name (Id))
import TIError (TIError (TIError))
import Types (Subst, Typ (TApp), Types (apply, ftv), tDouble, tInt)
import Unify (match, mgu)

-- | Types can be quantified by adding a list of predicates, to restrict
-- the ways in which type variables is instantiated.
--
-- Example of `Num a => a`(in haskell):
-- > [IsIn (TVar (TyVar (Id "a") KStar)) (Id "Num")] :=> TVar (TyVar (Id "a") KStar)
data Qual t = [Pred] :=> t deriving (Eq)

data Pred = IsIn Typ Name deriving (Eq)

-- | Represents a type class by a pair of lists, one containing super class names,
-- and the second containing the instance declarations.
--
-- Example of `Ord`:
-- > ([Id "Eq"], [[] :=> IsIn tInt (Id "Ord")])
--   This example captures the fact that `Eq` is a super class of `Ord`,
--   and `Ord` has a instance for the type `Int`.
data Class = Class [Name] [Inst]

type Inst = Qual Pred

instance Show Pred where
  show (IsIn t@(TApp _ _) n) = concat [show n, " (", show t, ")"]
  show (IsIn t n) = concat [show n, " ", show t]

instance Show a => Show (Qual a) where
  show ([] :=> t) = show t
  show ([p] :=> t) = concat [show p, " => ", show t]
  show (ps :=> t) = concat ["(", intercalate ", " $ map show ps, ") => ", show t]

instance Types a => Types (Qual a) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  ftv (ps :=> t) = ftv ps `union` ftv t

instance Types Pred where
  apply s (IsIn t n) = IsIn (apply s t) n
  ftv (IsIn t _) = ftv t

mguPred :: Pred -> Pred -> Either TIError Subst
mguPred = lift mgu

matchPred :: Pred -> Pred -> Either TIError Subst
matchPred = lift match

-- | Transforms unify functions to work on `Pred`.
lift :: (Typ -> Typ -> Either TIError a) -> Pred -> Pred -> Either TIError a
lift f (IsIn t n) (IsIn t' n')
  | n == n' = f t t'
  | otherwise = Left $ TIError "classes differ"

data ClassEnv = ClassEnv
  { -- | Maps identifiers to class values, or Nothing if there is no such class.
    classes :: Name -> Maybe Class,
    defaults :: [Typ]
  }

-- | As the classes or instances are processed, the initial environment is
-- transformed to add new entries, and it is possible to result in errors,
-- like in the case of redefining class, and etc, so `Maybe` is used to
-- allow the possibility of errors.
type EnvTransformer = forall m. MonadFail m => ClassEnv -> m ClassEnv

-- | Combines two env transformers
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) f g ce = f ce >>= g

super :: ClassEnv -> Name -> [Name]
super ce n = case classes ce n of Just (Class supers _) -> supers; Nothing -> []

insts :: ClassEnv -> Name -> [Inst]
insts ce n = case classes ce n of Just (Class _ insts) -> insts; Nothing -> []

initialEnv :: ClassEnv
initialEnv =
  ClassEnv
    { classes = \_ -> fail "no such class",
      defaults = [tInt, tDouble]
    }

-- | Add a new class into the environment without checking.
modify :: ClassEnv -> Name -> Class -> ClassEnv
modify ce n cls = ce {classes = classes'}
  where
    classes' :: Name -> Maybe Class
    classes' n'
      | n == n' = Just cls
      | otherwise = classes ce n'

-- | Add a new class into the environment checking if there is already, and
-- if all super classes are defined, if not, it fails.
addClass :: Name -> [Name] -> EnvTransformer
addClass n supers ce
  | isJust (classes ce n) = fail "class redefinition"
  | not (all (isJust . classes ce) supers) = fail "super class not defined"
  | otherwise = return $ modify ce n (Class supers [])

-- | Add a new instance of class into the environment if the class is defined,
-- and if there is not the instance, if not, it fails.
addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn _ n) ce
  | isNothing (classes ce n) = fail "class not defined"
  | any (overlap p) qs = fail "instance redefinition"
  | otherwise = return $ modify ce n cls
  where
    insts' :: [Inst]
    insts' = insts ce n

    qs :: [Pred]
    qs = [q | (_ :=> q) <- insts']

    cls :: Class
    cls = Class (super ce n) $ (ps :=> p) : insts'

-- | Checks if two predicates overlap each other.
overlap :: Pred -> Pred -> Bool
overlap p q = isRight (mguPred p q)

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses
  where
    addCoreClasses :: EnvTransformer
    addCoreClasses =
      addClass (Id "Eq") []
        <:> addClass (Id "Ord") [Id "Eq"]
        <:> addClass (Id "Show") []
        <:> addClass (Id "Read") []
        <:> addClass (Id "Bounded") []
        <:> addClass (Id "Enum") []
        <:> addClass (Id "Functor") []
        <:> addClass (Id "Monad") []

    addNumClasses :: EnvTransformer
    addNumClasses =
      addClass (Id "Num") [Id "Eq", Id "Show"]
        <:> addClass (Id "Real") [Id "Num", Id "Ord"]
        <:> addClass (Id "Fractional") [Id "Num"]
        <:> addClass (Id "Integral") [Id "Real", Id "Enum"]
        <:> addClass (Id "RealFrac") [Id "Real", Id "Fractional"]
        <:> addClass (Id "Floating") [Id "Fractional"]
        <:> addClass (Id "RealFloat") [Id "RealFrac", Id "Floating"]
