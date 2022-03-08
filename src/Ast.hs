module Ast
  ( Lit (..),
    Exp (..),
    Pat (..),
    Decl (..),
    ReplExp (..),
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
  | PCon Name [Pat] -- <name> [<pat>]
  deriving (Show)

data ReplExp = REExp Exp | REDecl Decl deriving (Show)

data Decl
  = DImpl Name Alt
  | DVal Name Scheme
  deriving (Show)

-- | Specifies the left and right sides of a function definition.
data Alt = Alt {pats :: [Pat], exp :: Exp} deriving (Show)

newtype Program = Program {decls :: [Decl]} deriving (Show)

data Exp
  = ELit Lit -- <lit>
  | EVar Name -- <name>
  | EApp Exp Exp -- <exp> <exp>
  | ELet [(Name, Alt)] Exp -- let <name> = <exp> in <exp>
  deriving (Show)
