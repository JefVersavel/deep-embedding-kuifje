module BasicEmbeddingV6 where

import Control.Monad.State.Lazy
import qualified Data.Map as M
import Data.Semigroup as Semi

type S = M.Map String Type

data CL
  = Skip
  | Update StoreManipulation CL
  | If BL CL CL CL
  | While BL CL CL
  | ReturnI Arithmetic
  | ReturnB BL

data Expression = Boolean BL | Integer Arithmetic

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
  | VarB String

data Arithmetic
  = Add Arithmetic Arithmetic
  | Sub Arithmetic Arithmetic
  | Mul Arithmetic Arithmetic
  | VarI String
  | Lit Int

data StoreManipulation = Set String Expression

data Type = B Bool | I Int

instance Show Type where
  show (B b) = show b
  show (I i) = show i

type ReturnType = Maybe Type

evalExpression :: Expression -> State S Type
evalExpression (Boolean be) = do
  value <- evalBL be
  return $ B value
evalExpression (Integer ae) = do
  value <- evalArithmetic ae
  return $ I value

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
evalBL (VarB name) = do
  state <- get
  case M.lookup name state of
    Just (B n) -> return n
    Just (I n) ->
      error
        ("Variable " ++ name ++ " is of type Int but Bool was expected")
    Nothing ->
      error
        ( "Variable " ++ show name ++ " is not defined."
        )

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
evalArithmetic (VarI name) = do
  state <- get
  case M.lookup name state of
    Just (I n) -> return n
    Just (B n) ->
      error
        ("Variable " ++ name ++ " is of type Bool but Int was expected")
    Nothing ->
      error
        ( "Variable " ++ show name ++ " is not defined."
        )
evalArithmetic (Lit i) = return i

evalStoreManipulation :: StoreManipulation -> State S ()
evalStoreManipulation (Set name expr) = do
  value <- evalExpression expr
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

int :: Int -> Expression
int i = Integer $ Lit i

true :: Expression
true = Boolean Tru

false :: Expression
false = Boolean Fls

example1 :: CL
example1 =
  update (Set "y" (int 0))
    <> update (Set "z" true)
    <> while
      (G (VarI "x") (Lit 0))
      ( update (Set "y" (Integer $ Add (VarI "y") (VarI "x")))
          <> update (Set "x" (Integer $ Sub (VarI "x") (Lit 1)))
      )
    <> ReturnI (VarI "y")

mainEval :: CL -> S -> (ReturnType, S)
mainEval expr = runState (eval expr)

testv6 = mainEval example1 start
  where
    start = M.insert "x" (I 6) M.empty
