{-# LANGUAGE FlexibleContexts #-}
module HasTEE where

import Control.Monad.State.Class
import Control.Monad.State.Strict

{-

liftNewRef :: a -> App (Server (Ref a))
newRef :: a -> Server (Ref a)
readRef    :: Ref a -> Server a
writeRef   :: Ref a -> a -> Server ()
-- immutable value
serverConstant :: a -> App (Server a)
-- closures
-- create an escape hatch that can be used however many times you want
remote :: Remotable a => a -> App (Remote a)

-- use the below function to introduce the Client monad
runClient :: Client () -> App Done

onServer :: Remote (Server a) -> Client a
(<.>) :: Binary a => Remote (a -> b) -> a -> Remote b

-- call this from `main` to run the App monad
runApp :: App a -> IO a



-}

type Name = String

data Exp = Lit Int
         | Var Name
         | Lam Name Exp
         | App Exp Exp

         -- HasTEE operators
         | Remote Exp
         deriving (Show)

data Value = IntVal Int
           | Closure Name Exp MachineEnv
           -- HasTEE values
           | RemoteClosure Name

           -- Error values
           | Err ErrState
           deriving (Show)

data ErrState = ENotClosure
              | EVarNotFound

instance Show ErrState where
  show ENotClosure  = "Closure not found"
  show EVarNotFound = "Variable not in environment"

type Env = [(Name, Value)]

type ClientEnv  = Env
type EnclaveEnv = Env

type MachineEnv   = (ClientEnv, EnclaveEnv)
type MachineState = (Value, MachineEnv)


type VarName = Int

interpret :: Exp -> Value
interpret e = fst $ evalState (eval e initEnv) initVarName
  where
    initEnv = ([],[])
    initVarName = 0

genRemVar :: (MonadState VarName m) => m String
genRemVar = do
  n <- get
  put (n + 1)
  pure ("RemVar" <> show n)

eval :: (MonadState VarName m)
     => Exp
     -> MachineEnv -> m MachineState
eval (Lit n) env   = pure (IntVal n, env)
eval (Var x) env@(clenv, _) = pure (lookupVar x clenv, env)
eval (Lam x e) env =
  pure (Closure x e env, env)
eval (App e1 e2) env = do
  (v1, env1) <- eval e1 env
  (v2, env2) <- eval e2 env1
  case v1 of
    Closure x body (cle, ence) ->
      eval body (((x, v2) : cle), ence)
    _ -> pure (Err ENotClosure, env2)
eval (Remote e) env = do
  (e', env1@(clenv, encenv)) <- eval e env
  case e' of
    Closure _ _ _ -> do
      varname <- genRemVar
      let encenv' = (varname, e') : encenv
      pure(RemoteClosure varname, (clenv, encenv'))
    _ -> pure (Err ENotClosure, env1)


lookupVar :: String -> [(String, Value)] -> Value
lookupVar _ [] = Err EVarNotFound
lookupVar x ((y, v) : env) =
  if x == y then v else lookupVar x env

-- type Map k v = [(k , v)]

-- member :: (Eq k) => k -> Map k v -> Bool
-- member _ [] = False
-- member k ((k1,_):xs)
--   | k == k1 = True
--   | otherwise = member k xs

-- add :: k -> v -> Map k v -> Map k v
-- add k val xs = (k, val) : xs

-- (~>) :: (Eq k) => Map k v -> k -> v
-- (~>) [] _ = error "Key not found"
-- (~>) ((i, val):xs) k
--   | i == k = val
--   | otherwise = xs ~> k
