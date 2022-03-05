module Ast (Lit (..), Decl (..), Exp (..)) where

import Name (Name)
import Types (Typ)

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
