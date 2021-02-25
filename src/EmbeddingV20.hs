module EmbeddingV20 where

import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup as Semi
import DistCalculations
import Language.Kuifje.Distribution as D
import qualified PrettyPrinting as P

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
  | ProbBool Prob BL BL

data Arithmetic
  = Add Arithmetic Arithmetic
  | Sub Arithmetic Arithmetic
  | Mul Arithmetic Arithmetic
  | Var String
  | Lit Int

data StoreManipulation = Set String Arithmetic | ProbAdd Prob StoreManipulation StoreManipulation

evalBL :: BL -> S -> Dist Bool
evalBL (E l r) s = point $ evalArithmetic l s == evalArithmetic r s
evalBL (G l r) s = point $ evalArithmetic l s > evalArithmetic r s
evalBL (GE l r) s = point $ evalArithmetic l s >= evalArithmetic r s
evalBL (L l r) s = point $ evalArithmetic l s < evalArithmetic r s
evalBL (LE l r) s = point $ evalArithmetic l s <= evalArithmetic r s
evalBL (And l r) s = binaryDistMap (&&) (evalBL l s) (evalBL r s)
evalBL (Or l r) s = binaryDistMap (||) (evalBL l s) (evalBL r s)
evalBL (Not l) s = unaryDistMap not $ evalBL l s
evalBL Fls _ = point False
evalBL Tru _ = point True
evalBL (ProbBool p l r) s = probAdd p (evalBL l s) (evalBL r s)

evalArithmetic :: Arithmetic -> S -> Int
evalArithmetic (Add l r) s = evalArithmetic l s + evalArithmetic r s
evalArithmetic (Sub l r) s = evalArithmetic l s - evalArithmetic r s
evalArithmetic (Mul l r) s = evalArithmetic l s * evalArithmetic r s
evalArithmetic (Var name) s = case M.lookup name s of
  Just n -> n
  Nothing ->
    error
      ( "Variable " ++ show name ++ " is not defined."
      )
evalArithmetic (Lit i) _ = i

evalStoreManipulation :: StoreManipulation -> S -> Dist S
evalStoreManipulation (Set name value) s = uniform [M.insert name (evalArithmetic value s) s]
evalStoreManipulation (ProbAdd p l r) s = probAdd p (evalStoreManipulation l s) (evalStoreManipulation r s)

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

f >=> g = D.join . D.fmap g . f

conditional :: BL -> (S -> Dist S) -> (S -> Dist S) -> (S -> Dist S)
conditional test trueEval falseEval =
  (\s -> D.fmap (\b -> (b, s)) (evalBL test s))
    >=> (\(b, s) -> if b then trueEval s else falseEval s)

eval :: CL -> (S -> Dist S)
eval Skip = point
eval (Update storeMan rest) = evalStoreManipulation storeMan >=> eval rest
eval (If test ifCL thenCL rest) = conditional test (eval ifCL) (eval thenCL) >=> eval rest
eval (While test whileCL rest) =
  conditional test (eval whileCL >=> eval (While test whileCL rest)) (eval rest)

example1 :: CL
example1 =
  update (Set "y" (Lit 0))
    <> while
      (G (Var "x") (Lit 0))
      ( update (Set "y" (Add (Var "y") (Var "x")))
          <> update (ProbAdd (2 / 3) (Set "x" (Sub (Var "x") (Lit 1))) (Set "x" (Sub (Var "x") (Lit 2))))
      )

testV20 = P.printDist $ eval example1 start
  where
    start = M.singleton "x" 3