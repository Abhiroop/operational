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
         | Let Name Exp Exp
         | Plus Exp Exp

         -- HasTEE operators
         | Remote    Exp
         | OnServer  Exp
         | RemoteApp Exp Exp -- (<.>)
         deriving (Show)

data Value = IntVal Int
           | Closure Name Exp Env
           -- HasTEE values
           | RemoteClosure Name
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

evalEnclave :: (MonadState StateVar m)
            => Exp -> EnclaveEnv -> m (Value, EnclaveEnv)
evalEnclave (Lit n) env = pure (IntVal n, env)
evalEnclave (Var x) env = pure (lookupVar x env, env)
evalEnclave (Lam x e) env =
  pure (Closure x e env, env)
evalEnclave (Let name e1 e2) env = do
  (e1', env') <- evalEnclave e1 env
  evalEnclave e2 ((name,e1'):env')
evalEnclave (App e1 e2) env = do
  (v1, env1) <- evalEnclave e1 env
  (v2, env2) <- evalEnclave e2 env1
  case v1 of
    Closure x body ev ->
      evalEnclave body ((x, v2):ev)
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
  pure (RemoteClosure varname, env'')
-- the following two are the essentially no-ops
evalEnclave (OnServer e) env = evalEnclave e env
evalEnclave (RemoteApp e1 e2) env = do
  (_, env1) <- evalEnclave e1 env
  (_, env2) <- evalEnclave e2 env1
  pure (Dummy, env2)


eval :: (MonadState StateVar m)
     => Exp -> ClientEnv -> m (Value, ClientEnv)
eval (Lit n) env = pure (IntVal n, env)
eval (Var x) env = pure (lookupVar x env, env)
eval (Lam x e) env =
  pure (Closure x e env, env)
eval (Let name e1 e2) env = do
  (e1', env') <- eval e1 env
  eval e2 ((name,e1'):env')
eval (App e1 e2) env = do
  (v1, env1) <- eval e1 env
  (v2, env2) <- eval e2 env1
  case v1 of
    Closure x body ev ->
      eval body ((x, v2):ev)
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
  pure (RemoteClosure varname, env'')
eval (OnServer e) env = do
  (e', env1) <- eval e env
  case e' of
    RemoteClosure varname -> pure (RemoteClosure varname, env1)
    _ -> pure (Err ENotRemClos, env1)
eval (RemoteApp e1 e2) env = do
  (v1, env1) <- eval e1 env
  (v2, env2) <- eval e2 env1
  case v1 of
    (RemoteClosure varname) -> do
      enclaveEnv <- gets encState
      let closure = lookupVar varname enclaveEnv
      case closure of
        Closure var body environ -> do
          (res,enclaveEnv') <- evalEnclave body ((var, v2):environ)
          --XXX: do we ever mutate the enclaveEnv ? Ideally no
          --XXX: So can we throw away enclaveEnv'? Things to see
          pure (res, env2)
        _ -> pure (Err ENotClosure, env2)
    _ -> pure (Err ENotRemClos, env2)




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
