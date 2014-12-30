module Main (main) where

import Parser

main :: IO ()

main = do
          print (parseExpr "g xs f v where g = \\xs f v. case xs of Nil -> v | Cons(x1,xs1) -> f x1 (g xs1 f v)")
          putStrLn ""
          print (parseExpr "case x of C1 -> Nil | C2 -> f xs where f = \\xs. case xs of Nil -> Nil | Cons(x1,xs1) -> Cons(x1, f xs1)")
          putStrLn ""
          print (parseExpr "f x1 x2 (let v3 = x3 in v3)")
          putStrLn ""
          print (parseExpr "g x where g = \\x. case x of C1 -> Nil | C2 -> f xs where f = \\xs. case xs of Nil -> Nil | Cons(x1,xs1) -> Cons(g x1, f xs1)")

