module Ast (Lit (..), Decl (..), Exp (..), Pat (..), BindGroup, Impl, Expl, Alt, Program) where

import Assump (Assump)
import Name (Name)
import Scheme (Scheme)
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

-- | Specifies the left and right sides of a function definition.
type Alt = ([Pat], Exp)

-- | Specifies a function definition: <name> [<pat>] = <exp>.
type Expl = (Name, Scheme, [Alt])

type BindGroup = ([Expl], [[Impl]])

type Impl = (Name, [Alt])

type Program = [BindGroup]

data Exp
  = ELit Lit -- <lit>
  | EVar Name -- <name>
  | EConst Assump
  | EApp Exp Exp -- <exp> <exp>
  | ELet BindGroup Exp -- let <name> = <exp> in <exp>
