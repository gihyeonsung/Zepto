module Ast where

data Stmt
  = Expr Expr
  deriving (Show)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Assign Var Expr
  | Lit Lit
  | Var Var
  deriving (Show)

data Lit
  = Num Integer
  | Bool Bool
  | String String
  deriving (Show)

newtype Var = V String deriving (Show)
