module DistCalculations where

import Data.Map as M
import Language.Kuifje.Distribution as D

binaryDistMap :: (Ord a, Ord b) => (a -> a -> b) -> Dist a -> Dist a -> Dist b
binaryDistMap op l r = l D.>>= (\left -> r D.>>= (point . op left))

unaryDistMap :: (Ord a, Ord b) => (a -> b) -> Dist a -> Dist b
unaryDistMap op d = d D.>>= (point . op)

probAdd :: Ord a => Prob -> Dist a -> Dist a -> Dist a
probAdd p l r = D $ unionWith (+) (M.map (p *) $ runD l) (M.map ((1 - p) *) $ runD r)
