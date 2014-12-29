module Main (main) where

import Parser

main :: IO ()

main = do
          print (parseExpr "f x y where f = \\x y. case x of Ca -> x | Cb -> y")
