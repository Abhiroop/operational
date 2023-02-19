{-# LANGUAGE FlexibleContexts #-}
module HasTEEOrig where

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
         | Fun [Name] Exp
         | App Exp [Exp]
         | Let Name Exp Exp
         | Plus Exp Exp

         -- HasTEE operators
         | Remote    Exp
         | OnServer  Exp
         | RemoteApp Exp Exp -- (<.>)
         deriving (Show)

data Value = IntVal Int
           | Closure [Name] Exp Env
           -- HasTEE values
           | RemoteClosure Name [Value]
           | ArgList [Value]
           | Dummy

           -- Error values
           | Err ErrState
           deriving (Show)

data ErrState = ENotClosure
              | EVarNotFound
              | ENotRemClos
              | ENotIntLit

instance Show ErrState where
  show ENotClosure  = "Closure not found"
  show EVarNotFound = "Variable not in environment"
  show ENotRemClos  = "Remote Closure not found"
  show ENotIntLit   = "Not an integer literal"

type Env = [(Name, Value)]

type ClientEnv  = Env
type EnclaveEnv = Env

type MachineEnv   = (ClientEnv, EnclaveEnv)
type MachineState = (Value, MachineEnv)


type VarName = Int


data StateVar =
  StateVar { varName  :: Int
           , encState :: EnclaveEnv
           }

initStateVar :: EnclaveEnv -> StateVar
initStateVar = StateVar 0


interpret :: Exp -> Value
interpret e =
  let newEnclaveEnv = snd $
                      evalState (evalEnclave e initEnclaveEnv)
                      (initStateVar initEnclaveEnv)
  in fst $ evalState (eval e initClientEnv) (initStateVar newEnclaveEnv)
  where
    initEnclaveEnv = []
    initClientEnv  = []

genRemVar :: (MonadState StateVar m) => m String
genRemVar = do
  n <- gets varName
  modify $ \s -> s {varName = 1 + n}
  pure ("RemVar" <> show n)

evalList :: (MonadState StateVar m) => [Exp] -> Env -> [Value] -> m ([Value], Env)
evalList []      e vals = pure (reverse vals, e)
evalList (e1:es) env xs = do
  (v, e) <- evalEnclave e1 env
  evalList es e (v:xs)


evalEnclave :: (MonadState StateVar m)
            => Exp -> EnclaveEnv -> m (Value, EnclaveEnv)
evalEnclave (Lit n) env = pure (IntVal n, env)
evalEnclave (Var x) env = pure (lookupVar x env, env)
evalEnclave (Fun xs e) env =
  pure (Closure xs e env, env)
evalEnclave (Let name e1 e2) env = do
  (e1', env') <- evalEnclave e1 env
  evalEnclave e2 ((name,e1'):env')
evalEnclave (App f args) env = do
  (v1, env1) <- evalEnclave f env
  (vals, env2) <- evalList args env1 []
  case v1 of
    Closure xs body ev ->
      evalEnclave body ((zip xs vals) ++ ev)
    _ -> pure (Err ENotClosure, env2)
evalEnclave (Plus e1 e2) env = do
  (v1, env1) <- evalEnclave e1 env
  (v2, env2) <- evalEnclave e2 env1
  case v1 of
    (IntVal a1) -> case v2 of
                  (IntVal a2) -> pure (IntVal (a1 + a2), env2)
                  _ -> pure (Err ENotIntLit, env2)
    _ -> pure (Err ENotIntLit, env2)

evalEnclave (Remote e) env = do
  (val, env') <- evalEnclave e env
  varname     <- genRemVar
  let env'' = (varname, val):env'
  pure (Dummy, env'')
-- the following two are the essentially no-ops
evalEnclave (OnServer e) env = evalEnclave e env
evalEnclave (RemoteApp e1 e2) env = do
  (_, env1) <- evalEnclave e1 env
  (_, env2) <- evalEnclave e2 env1
  pure (Dummy, env2)

evalList2 :: (MonadState StateVar m) => [Exp] -> Env -> [Value] -> m ([Value], Env)
evalList2 []      e vals = pure (reverse vals, e)
evalList2 (e1:es) env xs = do
  (v, e) <- eval e1 env
  evalList2 es e (v:xs)


eval :: (MonadState StateVar m)
     => Exp -> ClientEnv -> m (Value, ClientEnv)
eval (Lit n) env = pure (IntVal n, env)
eval (Var x) env = pure (lookupVar x env, env)
eval (Fun xs e) env =
  pure (Closure xs e env, env)
eval (Let name e1 e2) env = do
  (e1', env') <- eval e1 env
  eval e2 ((name,e1'):env')
eval (App f args) env = do
  (v1, env1) <- eval f env
  (v2, env2) <- evalList2 args env1 []
  case v1 of
    Closure xs body ev ->
      eval body ((zip xs v2) ++ ev)
    _ -> pure (Err ENotClosure, env2)
eval (Plus e1 e2) env = do
  (v1, env1) <- eval e1 env
  (v2, env2) <- eval e2 env1
  case v1 of
    (IntVal a1) -> case v2 of
                  (IntVal a2) -> pure (IntVal (a1 + a2), env2)
                  _ -> pure (Err ENotIntLit, env2)
    _ -> pure (Err ENotIntLit, env2)


eval (Remote e) env = do
  (_, env') <- eval e env
  varname     <- genRemVar
  let env'' = (varname, Dummy):env'
  pure (RemoteClosure varname [], env'')
eval (OnServer e) env = do
  (e', env1) <- eval e env
  case e' of
    RemoteClosure varname vals -> do
      enclaveEnv <- gets encState
      let func = lookupVar varname enclaveEnv
      case func of
        Closure vars body environ -> do
          (res,enclaveEnv') <- evalEnclave body ((zip vars vals) ++ environ)
          pure (res, env1)
        _ -> pure (Err ENotClosure, env1)
    _ -> pure (Err ENotRemClos, env1)
eval (RemoteApp e1 e2) env = do
  (v1, env1) <- eval e1 env
  (v2, env2) <- eval e2 env1
  case v1 of
    RemoteClosure f args ->
      case v2 of
        ArgList vals -> pure (RemoteClosure f (args ++ vals), env2)
        v -> pure (RemoteClosure f (args ++ [v]), env2)
    v -> pure (ArgList [v,v2], env2)


-- onServer (f == RemoteClosure f [])
-- onServer (f <.> arg == RA f arg == RemoteClosure f [arg])
-- onServer (f <.> arg1 <.> arg2 == RA f (RA arg1 arg2) == RC f [arg1, arg2])




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
