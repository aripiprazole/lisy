module Ast
  ( Lit (..),
    Exp (..),
    Pat (..),
    BindGroup (..),
    Expl (..),
    Alt (..),
    Program (..),
  )
where

import Assump (Assump)
import Name (Name)
import Scheme (Scheme)
import Types (Typ)

data Lit
  = LInt Int
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

newtype BindGroup = BindGroup {expls :: [Expl]} deriving (Show)

-- | Specifies a function definition: <name> [<pat>] = <exp>.
data Expl = Expl
  { eName :: Name,
    eAlts :: [Alt]
  }
  deriving (Show)

-- | Specifies the left and right sides of a function definition.
data Alt = Alt {pats :: [Pat], exp :: Exp} deriving (Show)

newtype Program = Program [BindGroup] deriving (Show)

data Exp
  = ELit Lit -- <lit>
  | EVar Name -- <name>
  | EApp Exp Exp -- <exp> <exp>
  | ELet BindGroup Exp -- let <name> = <exp> in <exp>
  deriving (Show)
