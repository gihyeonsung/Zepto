module Main where

import Ast

main :: IO ()
main =
  let ast = Expr $ Add 10 20 in
  print ast
