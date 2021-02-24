module DistCalculations where

import Data.Map
import Language.Kuifje.Distribution

binary :: (Ord a, Ord b) => (a -> a -> b) -> Dist a -> Dist a -> Dist b
binary op l r = D $ fromListWith (+) [(x `op` y, px * py) | (x, px) <- leftList, (y, py) <- rightList]
  where
    leftList = toList $ runD l
    rightList = toList $ runD r

unary :: (Ord a, Ord b) => (a -> b) -> Dist a -> Dist b
unary op d = D $ fromListWith (+) [(op x, p) | (x, p) <- list]
  where
    list = toList $ runD d

(+++) :: Dist Int -> Dist Int -> Dist Int
l +++ r = binary (+) l r

(~~~) :: Dist Int -> Dist Int -> Dist Int
l ~~~ r = binary (-) l r

(***) :: Dist Int -> Dist Int -> Dist Int
l *** r = binary (*) l r

(===) :: Dist Int -> Dist Int -> Dist Bool
l === r = binary (==) l r

(>>>) :: Dist Int -> Dist Int -> Dist Bool
l >>> r = binary (>) l r

(>>>=) :: Dist Int -> Dist Int -> Dist Bool
l >>>= r = binary (>=) l r

(<<<) :: Dist Int -> Dist Int -> Dist Bool
l <<< r = binary (<) l r

(<<<=) :: Dist Int -> Dist Int -> Dist Bool
l <<<= r = binary (<=) l r

(&&&) :: Dist Bool -> Dist Bool -> Dist Bool
l &&& r = binary (&&) l r

(|||) :: Dist Bool -> Dist Bool -> Dist Bool
l ||| r = binary (||) l r
