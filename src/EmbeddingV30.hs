{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module EmbeddingV30 where

import qualified Control.Monad as C
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup as Semi
import DistCalculations
import Language.Kuifje.Distribution as D
import qualified PrettyPrinting as P

type S = M.Map String Int

type Bit = Bool

type Bits = [Bit]

type a ===> b = a -> DB b

type DB a = Dist (Bits, a)

class ToBits a where
  toBits :: a -> Bits

instance ToBits Int where
  toBits n = (n < 0) : unfoldr (\m -> if m == 0 then Nothing else Just (odd m, quot m 2)) n

instance ToBits Bool where
  toBits b = [b]

data CL
  = Skip
  | Update StoreManipulation CL
  | If BL CL CL CL
  | While BL CL CL
  | Observe BitsLang CL

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
  | ProbAri Prob Arithmetic Arithmetic

data BitsLang = Ari Arithmetic | ProbAddition Prob Arithmetic Arithmetic

data StoreManipulation = Set String Arithmetic | ProbAdd Prob StoreManipulation StoreManipulation

evalBitsLang :: BitsLang -> S -> Dist Bits
evalBitsLang (Ari a) s = D.fmap toBits $ evalArithmetic a s

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
evalBL (ProbBool p l r) s = probAdd p (evalBL l s) (evalBL r s)

evalArithmetic :: Arithmetic -> S -> Dist Int
evalArithmetic (Add l r) s = binaryDistMap (+) (evalArithmetic l s) (evalArithmetic r s)
evalArithmetic (Sub l r) s = binaryDistMap (-) (evalArithmetic l s) (evalArithmetic r s)
evalArithmetic (Mul l r) s = binaryDistMap (*) (evalArithmetic l s) (evalArithmetic r s)
evalArithmetic (Var name) s = case M.lookup name s of
  Just n -> point n
  Nothing ->
    error
      ( "Variable " ++ show name ++ " is not defined."
      )
evalArithmetic (Lit i) _ = point i
evalArithmetic (ProbAri p l r) s = probAdd p (evalArithmetic l s) (evalArithmetic r s)

evalStoreManipulation :: StoreManipulation -> S -> Dist S
evalStoreManipulation (Set name value) s = setStoreDist s name $ evalArithmetic value s
evalStoreManipulation (ProbAdd p l r) s = probAdd p (evalStoreManipulation l s) (evalStoreManipulation r s)

setStoreDist :: S -> String -> Dist Int -> Dist S
setStoreDist s name val = D $ M.fromList $ map (\(v, p) -> (M.insert name v s, p)) dist
  where
    dist = M.toList $ runD val

instance Semigroup CL where
  Skip <> k = k
  Update f p <> k = Update f (p <> k)
  If c p q r <> k = If c p q (r <> k)
  While c p q <> k = While c p (q <> k)
  Observe f p <> k = Observe f (p <> k)

skip :: CL
skip = Skip

update :: StoreManipulation -> CL
update storeman = Update storeman skip

cond :: BL -> CL -> CL -> CL
cond c p q = If c p q skip

while :: BL -> CL -> CL
while c p = While c p skip

observe :: BitsLang -> CL
observe f = Observe f skip

f >=> g = join . D.fmap g . f

conditional :: BL -> (S ===> S) -> (S ===> S) -> (S ===> S)
conditional test trueEval falseEval =
  obsem (D.fmap toBits . evalBL test)
    >=> (\([b], s) -> let a = D.return (b, s) in a)
    >=> (\(b, s) -> if b then trueEval s else falseEval s)

data CLF a
  = SkipF
  | UpdateF StoreManipulation a
  | IfF BL a a a
  | WhileF BL a a
  | ObserveF BitsLang a

c :: CLF CL -> CL
c SkipF = Skip
c (UpdateF f p) = Update f p
c (IfF c p q r) = If c p q r
c (WhileF c p q) = While c p q
c (ObserveF f p) = Observe f p

propagate :: (CLF a -> a) -> (CL -> a)
propagate alg Skip = alg SkipF
propagate alg (Update f p) = alg (UpdateF f (propagate alg p))
propagate alg (If c p q r) = alg (IfF c (propagate alg p) (propagate alg q) (propagate alg r))
propagate alg (While c p q) = alg (WhileF c (propagate alg p) (propagate alg q))
propagate alg (Observe f p) = alg (ObserveF f (propagate alg p))

sem :: CL -> (S ===> S)
sem = propagate alg
  where
    alg :: CLF (S ===> S) -> (S ===> S)
    alg SkipF = \s -> D.return ([], s)
    alg (UpdateF f p) = uplift (evalStoreManipulation f) >=> (p . snd)
    alg (IfF c p q r) = conditional c p q >=> (r . snd)
    alg (WhileF c p q) =
      let while = conditional c (p >=> (while . snd)) q
       in while
    alg (ObserveF f p) = obsem (evalBitsLang f) >=> (p . snd)

uplift :: Ord b => (a -> Dist b) -> (a ===> b)
uplift f = D.fmap ([],) . f

obsem :: (S -> Dist Bits) -> (S ===> S)
obsem f s = D.fmap (,s) (f s)

example3a :: CL
example3a =
  update (Set "y" (Lit 0))
    <> while
      (G (Var "x") (Lit 0))
      ( update (Set "y" (Add (Var "y") (Var "x")))
          <> update (ProbAdd (2 / 3) (Set "x" (Sub (Var "x") (Lit 1))) (Set "x" (Sub (Var "x") (Lit 2))))
          <> observe (Ari (Var "x"))
      )

testV30 = sem example3a start
  where
    start = M.singleton "x" 3
