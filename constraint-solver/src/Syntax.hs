module Syntax where

type Name = String

data Exp
  = Var Name
  | App Exp Exp
  | Lam Name Exp
  | Let Name Exp Exp
  | Lit Lit
  | If Exp Exp Exp
  | Fix Exp
  | Op Binop Exp Exp
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Program = Program [Decl] Exp deriving Eq

type Decl = (String, Exp)
