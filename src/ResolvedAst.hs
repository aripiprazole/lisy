module ResolvedAst
  ( RExp (..),
    RPat (..),
    RBindGroup (..),
    RImpl (..),
    RExpl (..),
    RAlt (..),
    RProgram (..),
    bgFromTuples,
  )
where

import Assump (Assump)
import Ast (Lit)
import Name (Name)
import Scheme (Scheme)
import Types (Typ)

data RPat
  = RPVar Name
  | RPWildcard
  | RPAs Name RPat
  | RPLit Lit
  | RPNpk Name Int
  | RPCon Assump [RPat]
  deriving (Show, Eq)

-- | Specifies a function definition: <name> [<pat>] = <exp>.
data RExpl = RExpl
  { eName :: Name,
    eScheme :: Scheme,
    eAlts :: [RAlt]
  }
  deriving (Show, Eq)

data RExp
  = RELit Lit
  | REVar Name
  | REConst Assump
  | REApp RExp RExp
  | RELet RBindGroup RExp
  deriving (Show, Eq)

-- | Specifies the left and right sides of a function definition.
data RAlt = RAlt {pats :: [RPat], exp :: RExp} deriving (Show, Eq)

data RImpl = RImpl {iName :: Name, iAlts :: [RAlt]} deriving (Show, Eq)

data RBindGroup = RBindGroup {expls :: [RExpl], impls :: [[RImpl]]} deriving (Show, Eq)

newtype RProgram = RProgram [RBindGroup] deriving (Show, Eq)

bgFromTuples :: [(Name, Maybe Scheme, [RAlt])] -> RBindGroup
bgFromTuples g =
  RBindGroup [RExpl v t alts | (v, Just t, alts) <- g] $
    filter (not . null) [[RImpl v alts | (v, Nothing, alts) <- g]]