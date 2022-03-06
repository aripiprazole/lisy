module Types
  ( Subst,
    nullSubst,
    (+->),
    Types (..),
    (@@),
    merge,
    Typ (..),
    Kind (..),
    HasKind (..),
    TyCon (..),
    TyVar (..),
    tUnit,
    tChar,
    tInt,
    tDouble,
    tFloat,
    tString,
    tList,
    tArrow,
    tTuple2,
    list,
    pair,
    (-:>>),
    (->>),
    enumId,
  )
where

import Data.List (intersect, nub, union)
import Name (Name (..))

-- | Represents type variables's mapping to types
type Subst = [(TyVar, Typ)]

nullSubst :: Subst
nullSubst = []

(+->) :: TyVar -> Typ -> Subst
(+->) u t = [(u, t)]

-- | Composes substitutions,
-- `apply (s1 @@ s2)` is equal to `apply s1 . apply s2`.
-- We can't use `++` between substitutions because bindings in s1 takes
-- precedence of any bindings in s2. For that the `merge` function is used.
-- The result of both functions preserves the types' kinds.
(@@) :: Subst -> Subst -> Subst
(@@) s1 s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

-- | Merges two substitutions, is equivalent to `s1 ++ s2` but
-- if there are any conflicts between the type variables of the two
-- substitutions, it fails, otherwise, returns `s1 ++ s2`.
-- The result preserves the types' kinds.
merge :: MonadFail m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return $ s1 ++ s2 else fail "merge fails"
  where
    agree :: Bool
    agree = all f (map fst s1 `intersect` map fst s2)

    f :: TyVar -> Bool
    f u = apply s1 (TVar u) == apply s2 (TVar u)

-- | Represents simple types.
data Typ -- τ
  = TCon TyCon -- α
  | TVar TyVar -- α
  | TApp Typ Typ -- τ τ
  | -- | Represents quantified types.
    TGen Int
  deriving (Eq)

data TyCon = TyCon Name Kind deriving (Eq)

data TyVar = TyVar Name Kind deriving (Eq)

instance Show Typ where
  show (TGen i) = "v" ++ show i
  show (TApp a b) = concat ["(", show a, " ", show b, ")"]
  show (TCon (TyCon name _)) = show name
  show (TVar (TyVar name _)) = "'" ++ show name

-- | Types are represented with different kinds like Int, Char -> Bool.
-- Kinds like k1 -> k2 represents paramaterized types like Maybe and IO.
data Kind = KStar | KFun Kind Kind deriving (Eq)

class HasKind a where kind :: a -> Kind

instance HasKind TyCon where kind (TyCon _ k) = k

instance HasKind TyVar where kind (TyVar _ k) = k

instance HasKind Typ where
  kind (TGen _) = KStar
  kind (TCon tc) = kind tc
  kind (TVar u) = kind u
  kind (TApp a _) = case kind a of
    (KFun _ k) -> k
    KStar -> KStar

class Types a where
  -- | Replaces all ocurrences of type variables in a type with the
  -- corresponding types in the given substitution
  apply :: Subst -> a -> a

  -- | Returns a set of free variables of `a`
  ftv :: a -> [TyVar]

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv = nub . concatMap ftv

instance Types Typ where
  apply s (TVar u) = case lookup u s of
    Just t -> t
    Nothing -> TVar u
  apply s (TApp a b) = TApp (apply s a) (apply s b)
  apply s t = t

  ftv (TVar u) = [u]
  ftv (TApp a b) = ftv a `union` ftv b
  ftv t = []

tUnit :: Typ
tUnit = TCon (TyCon (Id "()") KStar)

tChar :: Typ
tChar = TCon (TyCon (Id "Char") KStar)

tInt :: Typ
tInt = TCon (TyCon (Id "Int") KStar)

tFloat :: Typ
tFloat = TCon (TyCon (Id "Float") KStar)

tDouble :: Typ
tDouble = TCon (TyCon (Id "Double") KStar)

tList :: Typ
tList = TCon (TyCon (Id "List") (KStar -:>> KStar))

tArrow :: Typ
tArrow = TCon (TyCon (Id "(->)") (KStar -:>> KStar -:>> KStar))

tTuple2 :: Typ
tTuple2 = TCon (TyCon (Id "(,)") (KStar -:>> KStar -:>> KStar))

tString :: Typ
tString = list tChar

list :: Typ -> Typ
list = TApp tList

pair :: Typ -> Typ -> Typ
pair a b = (tTuple2 `TApp` a) `TApp` b

(-:>>) :: Kind -> Kind -> Kind
(-:>>) = KFun

(->>) :: Typ -> Typ -> Typ
(->>) a b = (tArrow `TApp` a) `TApp` b

enumId :: Int -> Name
enumId n = Id ("v" ++ show n)