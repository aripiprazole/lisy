module Ast (Lit (..), Decl (..), Exp (..), Pat (..), BindGroup (..)) where

import Assump (Assump)
import Name (Name)
import Types (Typ)

data Lit
  = LInt Int
  | LChar Char
  | LString String
  | LRat Rational
  deriving (Eq, Ord, Show)

data Pat
  = PVar Name -- <name>
  | PWildcard -- _
  | PAs Name Pat -- <name>@<pat>
  | PLit Lit -- <lit>
  | PNpk Name Int -- <n> + <k>
  | PCon Assump [Pat] -- <name> [<pat>]

data Decl
  = DVal Name Typ -- <name> : <typ>
  | DLet Name [Pat] Exp -- <name> [<pat>] = <exp>

data BindGroup

data Exp
  = ELit Lit -- <lit>
  | EVar Name -- <name>
  | EConst Assump
  | EApp Exp Exp -- <exp> <exp>
  | ELet BindGroup Exp -- let <name> = <exp> in <exp>
