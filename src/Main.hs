module Main (main) where

import Parser

main :: IO ()

main = do
          print (parseExpr "g xs f v where g = \\xs f v. case xs of Nil -> v | Cons(x1,xs1) -> f x1 (g xs1 f v)")
