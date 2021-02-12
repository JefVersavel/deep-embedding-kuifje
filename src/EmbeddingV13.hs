module BasicEmbeddingV4 where

import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Semigroup as Semi

type S = M.Map String Int

data CL
  = Skip
  | Update StoreManipulation CL
  | If BL CL CL CL
  | While BL CL CL
  | ReturnI Arithmetic
  | ReturnB BL

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

data Type = B Bool | I Int

instance Show Type where
  show (B b) = show b
  show (I i) = show i

type ReturnType = Maybe Type

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
  ReturnI a <> k = ReturnI a
  ReturnB b <> k = ReturnB b

skip :: CL
skip = Skip

update :: StoreManipulation -> CL
update storeman = Update storeman skip

cond :: BL -> CL -> CL -> CL
cond c p q = If c p q skip

while :: BL -> CL -> CL
while c p = While c p skip

conditional :: BL -> State S ReturnType -> State S ReturnType -> State S ReturnType
conditional test trueEval falseEval = do
  bool <- evalBL test
  if bool
    then trueEval
    else falseEval

eval :: CL -> State S ReturnType
eval Skip = return Nothing
eval (Update storeMan rest) = do
  evalStoreManipulation storeMan
  eval rest
eval (If test ifCL thenCL rest) = do
  result <- conditional test (eval ifCL) (eval thenCL)
  case result of
    Nothing -> eval rest
    Just r -> return $ Just r
eval (While test whileCL rest) =
  conditional test trueEval (eval rest)
  where
    trueEval = do
      result <- eval whileCL
      case result of
        Nothing -> eval (While test whileCL rest)
        Just r -> return $ Just r
eval (ReturnI ari) = do
  value <- evalArithmetic ari
  return $ Just (I value)
eval (ReturnB bool) = do
  value <- evalBL bool
  return $ Just (B value)

example1 :: CL
example1 =
  update (Set "y" (Lit 0))
    <> while
      (G (Var "x") (Lit 0))
      ( update (Set "y" (Add (Var "y") (Var "x")))
          <> update (Set "x" (Sub (Var "x") (Lit 1)))
      )
    <> ReturnI (Var "y")

mainEval :: CL -> S -> (ReturnType, S)
mainEval expr = runState (eval expr)

testv4 = mainEval example1 start
  where
    start = M.insert "x" 6 M.empty
