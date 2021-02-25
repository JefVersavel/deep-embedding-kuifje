module EmbeddingV20 where

import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup as Semi
import DistCalculations
import Language.Kuifje.Distribution

type S = M.Map String (Dist Int)

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

data StoreManipulation = Set String Arithmetic | ProbAdd Prob StoreManipulation StoreManipulation

evalBL :: BL -> S -> Dist Bool
evalBL (E l r) s = binaryDistMap (==) (evalArithmetic l s) (evalArithmetic r s)
evalBL (G l r) s = binaryDistMap (>) (evalArithmetic l s) (evalArithmetic r s)
evalBL (GE l r) s = binaryDistMap (>=) (evalArithmetic l s) (evalArithmetic r s)
evalBL (L l r) s = binaryDistMap (<) (evalArithmetic l s) (evalArithmetic r s)
evalBL (LE l r) s = binaryDistMap (<=) (evalArithmetic l s) (evalArithmetic r s)
evalBL (And l r) s = binaryDistMap (&&) (evalBL l s) (evalBL r s)
evalBL (Or l r) s = binaryDistMap (||) (evalBL l s) (evalBL r s)
evalBL (Not l) s = unaryDistMap not $ evalBL l s
evalBL Fls _ = point False
evalBL Tru _ = point True

completeBoolDist :: Dist Bool -> Dist Bool
completeBoolDist d
  | M.size (runD d) == 2 = d
  | M.size (runD d) == 1 =
    let dd = runD d
     in case M.lookup False dd of
          Nothing -> D $ M.insert False (1 - fromJust (M.lookup True dd)) dd
          Just p -> D $ M.insert True (1 - p) dd
  | otherwise = uniform [True, False]

evalArithmetic :: Arithmetic -> S -> Dist Int
evalArithmetic (Add l r) s = binaryDistMap (+) (evalArithmetic l s) (evalArithmetic r s)
evalArithmetic (Sub l r) s = binaryDistMap (-) (evalArithmetic l s) (evalArithmetic r s)
evalArithmetic (Mul l r) s = binaryDistMap (*) (evalArithmetic l s) (evalArithmetic r s)
evalArithmetic (Var name) s = case M.lookup name s of
  Just n -> n
  Nothing ->
    error
      ( "Variable " ++ show name ++ " is not defined."
      )
evalArithmetic (Lit i) _ = point i

evalStoreManipulation :: StoreManipulation -> S -> S
evalStoreManipulation (Set name value) s = M.insert name (evalArithmetic value s) s
evalStoreManipulation (ProbAdd p l r) s =
  probStoreMerge
    p
    (evalStoreManipulation l s)
    (evalStoreManipulation r s)

probStoreMerge :: Prob -> S -> S -> S
probStoreMerge p = M.unionWith (probAdd p)

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

f >>>> g = g f

conditional :: BL -> (S -> S) -> (S -> S) -> S -> S
conditional test trueEval falseEval s = probStoreMerge tru (trueEval s) (falseEval s)
  where
    testResults = runD $ completeBoolDist $ evalBL test s
    tru = fromJust $ M.lookup True testResults

eval :: CL -> S -> S
eval Skip s = s
eval (Update storeMan rest) s = evalStoreManipulation storeMan s >>>> eval rest
eval (If test ifCL thenCL rest) s = conditional test (eval ifCL) (eval thenCL) s >>>> eval rest
eval (While test whileCL rest) s =
  conditional test (\s -> eval whileCL s >>>> eval (While test whileCL rest)) (eval rest) s

example1 :: CL
example1 =
  update (Set "y" (Lit 0))
    <> update (ProbAdd (1 / 2) (Set "x" (Lit 1)) (Set "x" (Lit 2)))

-- <> while
--   (G (Var "x") (Lit 0))
--   ( update (Set "y" (Add (Var "y") (Var "x")))
--       <> update (Set "x" (Sub (Var "x") (Lit 1)))
--   )

testV20 = eval example1 start
  where
    start = M.insert "x" (uniform [3]) M.empty
