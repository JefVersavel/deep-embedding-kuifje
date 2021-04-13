module CodePaper.EmbeddingV11 where

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

evalBL :: BL -> S -> Bool
evalBL (E l r) s = evalArithmetic l s == evalArithmetic r s
evalBL (G l r) s = evalArithmetic l s > evalArithmetic r s
evalBL (GE l r) s = evalArithmetic l s >= evalArithmetic r s
evalBL (L l r) s = evalArithmetic l s < evalArithmetic r s
evalBL (LE l r) s = evalArithmetic l s <= evalArithmetic r s
evalBL (And l r) s = evalBL l s && evalBL r s
evalBL (Or l r) s = evalBL l s || evalBL r s
evalBL (Not l) s = not $ evalBL l s
evalBL Fls _ = False
evalBL Tru _ = True

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

evalStoreManipulation :: StoreManipulation -> S -> S
evalStoreManipulation (Set name value) s = M.insert name (evalArithmetic value s) s

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

data CLF a
  = SkipF
  | UpdateF StoreManipulation a
  | IfF BL a a a
  | WhileF BL a a

c :: CLF CL -> CL
c SkipF = Skip
c (UpdateF f p) = Update f p
c (IfF c p q r) = If c p q r
c (WhileF c p q) = While c p q

propagate :: (CLF a -> a) -> (CL -> a)
propagate alg Skip = alg SkipF
propagate alg (Update f p) = alg (UpdateF f (propagate alg p))
propagate alg (If c p q r) = alg (IfF c (propagate alg p) (propagate alg q) (propagate alg r))
propagate alg (While c p q) = alg (WhileF c (propagate alg p) (propagate alg q))

f >>> g = g . f

conditional :: BL -> (S -> S) -> (S -> S) -> (S -> S)
conditional test trueEval falseEval =
  (\s -> (evalBL test s, s))
    >>> (\(b, s) -> if b then trueEval s else falseEval s)

--  evalBL test s = trueEval s
--  otherwise = falseEval s

sem :: CL -> (S -> S)
sem = propagate alg
  where
    alg :: CLF (S -> S) -> (S -> S)
    alg SkipF = id
    alg (UpdateF f p) = evalStoreManipulation f >>> p
    alg (IfF c p q r) = conditional c p q >>> r
    alg (WhileF c p q) =
      let while = conditional c (p >>> while) q
       in while

-- eval :: CL -> S -> S
-- eval Skip s = s
-- eval (Update storeMan rest)s = evalStoreManipulation storeMan s >>> eval rest
-- eval (If test ifCL thenCL rest) s = conditional test (eval ifCL) (eval thenCL) s >>> eval rest
-- eval (While test whileCL rest) s =
--   conditional test (\s -> eval whileCL s >>> eval (While test whileCL rest)) (eval rest) s

example1 :: CL
example1 =
  update (Set "y" (Lit 0))
    <> while
      (G (Var "x") (Lit 0))
      ( update (Set "y" (Add (Var "y") (Var "x")))
          <> update (Set "x" (Sub (Var "x") (Lit 1)))
      )

testV11 = sem example1 start
  where
    start = M.insert "x" 3 M.empty
