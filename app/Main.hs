module Main where

import HasTEE


prog :: Exp
prog = Lam "x" (Var "x")

prog1 :: Exp
prog1 =
  Let "m" (Lit 3)
  (Let "f" (Lam "x" (Plus (Var "x") (Var "m")))
   (Let "y" (Remote (Var "f"))
    (RemoteApp (OnServer (Var "y")) (Lit 2))))

main :: IO ()
main = print $ interpret prog1
