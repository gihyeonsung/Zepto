module Ast where

data Expr = Add Integer Integer deriving (Show)

data Stmt = Expr Expr deriving (Show)
