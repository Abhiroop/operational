module Main where

import HasTEE


prog :: Exp
prog = Lam "x" (Var "x")

main :: IO ()
main = print $ interpret prog
