module Ast
  ( Lit (..),
    Decl (..),
    Exp (..),
    Pat (..),
    BindGroup (..),
    Impl (..),
    Expl (..),
    Alt (..),
    Program (..),
    HasName (..),
    alts,
    pats,
  )
where

import Assump (Assump)
import Name (Name)
import Scheme (Scheme)
import Types (Typ)

data Lit
  = LInt Int
  | LChar Char
  | LString String
  | LRat Rational
  | LUnit
  deriving (Eq, Ord, Show)

data Pat
  = PVar Name -- <name>
  | PWildcard -- _
  | PAs Name Pat -- <name>@<pat>
  | PLit Lit -- <lit>
  | PNpk Name Int -- <n> + <k>
  | PCon Assump [Pat] -- <name> [<pat>]
  deriving (Show)

data Decl
  = DVal Name Typ -- <name> : <typ>
  | DLet Name [Pat] Exp -- <name> [<pat>] = <exp>
  deriving (Show)

-- | Specifies the left and right sides of a function definition.
data Alt = Alt [Pat] Exp deriving (Show)

-- | Specifies a function definition: <name> [<pat>] = <exp>.
data Expl = Expl Name Scheme [Alt] deriving (Show)

data BindGroup = BindGroup [Expl] [[Impl]] deriving (Show)

data Impl = Impl Name [Alt] deriving (Show)

newtype Program = Program [BindGroup] deriving (Show)

data Exp
  = ELit Lit -- <lit>
  | EVar Name -- <name>
  | EConst Assump
  | EApp Exp Exp -- <exp> <exp>
  | ELet BindGroup Exp -- let <name> = <exp> in <exp>
  deriving (Show)

pats :: Alt -> [Pat]
pats (Alt ps _) = ps

alts :: Impl -> [Alt]
alts (Impl _ alts) = alts

class HasName a where name :: a -> Name

instance HasName Expl where name (Expl n _ _) = n

instance HasName Impl where name (Impl n _) = n