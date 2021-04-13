module Boolean where

import Arithmetic
import State

data OpAriBool = S | G | Se | Ge | E | NE
  deriving (Show)

data UnOpBool = N
  deriving (Show)

data BinOpBool = And | Or
  deriving (Show)

opAriBoolTofunc :: (Ord a) => OpAriBool -> (a -> a -> Bool)
opAriBoolTofunc S = (<)
opAriBoolTofunc G = (>)
opAriBoolTofunc Se = (<=)
opAriBoolTofunc Ge = (>=)
opAriBoolTofunc E = (==)
opAriBoolTofunc NE = (/=)

unOpBoolToFunc :: UnOpBool -> (Bool -> Bool)
unOpBoolToFunc N = not

binOpBoolToFunc :: BinOpBool -> (Bool -> Bool -> Bool)
binOpBoolToFunc And = (&&)
binOpBoolToFunc Or = (||)
