module BasicEmbeddingV3 where

import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Semigroup as Semi

type S = M.Map String Int

data CL
  = Skip
  | Update StoreManipulation CL
  | If BL CL CL CL
  | While BL CL CL

data BL
  = E Arithmetic Arithmetic
  | G Arithmetic Arithmetic
  | GE Arithmetic Arithmetic
  | L Arithmetic Arithmetic
  | LE Arithmetic Arithmetic
  | And BL BL
  | Or BL BL
  | Not BL
  | Fls
  | Tru

data Arithmetic
  = Add Arithmetic Arithmetic
  | Sub Arithmetic Arithmetic
  | Mul Arithmetic Arithmetic
  | Var String
  | Lit Int

data StoreManipulation = Set String Arithmetic

evalBL :: BL -> State S Bool
evalBL (E l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left == right
evalBL (G l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left > right
evalBL (GE l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left >= right
evalBL (L l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left < right
evalBL (LE l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left <= right
evalBL (And l r) = do
  left <- evalBL l
  right <- evalBL r
  return $ left && right
evalBL (Or l r) = do
  left <- evalBL l
  right <- evalBL r
  return $ left || right
evalBL (Not l) = do
  left <- evalBL l
  return $ not left
evalBL Fls = return False
evalBL Tru = return True

evalArithmetic :: Arithmetic -> State S Int
evalArithmetic (Add l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left + right
evalArithmetic (Sub l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left - right
evalArithmetic (Mul l r) = do
  left <- evalArithmetic l
  right <- evalArithmetic r
  return $ left * right
evalArithmetic (Var name) = do
  state <- get
  case M.lookup name state of
    Just n -> return n
    Nothing ->
      error
        ( "Variable " ++ show name ++ " is not defined."
        )
evalArithmetic (Lit i) = return i

evalStoreManipulation :: StoreManipulation -> State S ()
evalStoreManipulation (Set name ari) = do
  value <- evalArithmetic ari
  state <- get
  put $ M.insert name value state

instance Semigroup CL where
  Skip <> k = k
  Update f p <> k = Update f (p <> k)
  If c p q r <> k = If c p q (r <> k)
  While c p q <> k = While c p (q <> k)

skip :: CL
skip = Skip

update :: StoreManipulation -> CL
update storeman = Update storeman skip

cond :: BL -> CL -> CL -> CL
cond c p q = If c p q skip

while :: BL -> CL -> CL
while c p = While c p skip

conditional :: BL -> State S () -> State S () -> State S ()
conditional test trueEval falseEval = do
  bool <- evalBL test
  if bool
    then trueEval
    else falseEval

eval :: CL -> State S ()
eval Skip = return ()
eval (Update storeMan rest) = do
  evalStoreManipulation storeMan
  eval rest
eval (If test ifCL thenCL rest) = do
  conditional test (eval ifCL) (eval thenCL)
  eval rest
eval (While test whileCL rest) =
  conditional test trueEval (eval rest)
  where
    trueEval = do
      eval whileCL
      eval (While test whileCL rest)

example1 :: CL
example1 =
  update (Set "y" (Lit 0))
    <> while
      (G (Var "x") (Lit 0))
      ( update (Set "y" (Add (Var "y") (Var "x")))
          <> update (Set "x" (Sub (Var "x") (Lit 1)))
      )

mainEval :: CL -> S -> S
mainEval expr = execState (eval expr)

testv3 = mainEval example1 start
  where
    start = M.insert "x" 6 M.empty
