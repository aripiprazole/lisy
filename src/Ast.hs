module Ast
  ( Name (..),
    Lit (..),
    Decl (..),
    Exp (..),
    Kind (..),
    Typ (..),
    TyCon (..),
    TyVar (..),
    HasKind (..),
    tUnit,
    tChar,
    tInt,
    tFloat,
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

newtype Name = Id String deriving (Eq, Ord)

instance Show Name where
  show (Id s) = s

data Lit
  = LInt Int
  | LBool Bool
  | LChar Char
  | LString String
  | LFloat Float
  deriving (Eq, Ord, Show)

type Pat = Name

data Decl
  = DVal Name Typ -- <name> : <typ>
  | DLet Name [Pat] Exp -- <name> [<pat>] = <exp>

data Exp
  = ELit Lit -- <lit>
  | EVar Name -- <name>
  | EApp Exp Exp -- <exp> <exp>
  | ELet Name Exp Exp -- let <name> = <exp> in <exp>
  | EAbs Name Exp -- \<name> -> <exp>

-- | Types are represented with different kinds like Int, Char -> Bool.
-- Kinds like k1 -> k2 represents paramaterized types like Maybe and IO.
data Kind = KStar | KFun Kind Kind deriving (Eq)

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