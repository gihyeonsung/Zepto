module Main where

import Ast

main :: IO ()
main =
  let ast = Expr $ Add (Lit $ Num 10) (Lit $ Num 20)
   in print ast
