module Main where

import HasTEEOrig


prog :: Exp
prog = Fun ["x"] (Var "x")

prog1 :: Exp
prog1 =
  Let "m" (Lit 3)
  (Let "f" (Fun ["x"] (Plus (Var "x") (Var "m")))
   (Let "y" (InEnclave (Var "f"))
    (Gateway (EnclaveApp (Var "y") (Lit 2)))))

main :: IO ()
main = print $ eval prog1
