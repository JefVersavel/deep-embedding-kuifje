module Boolean where

import Arithmetic
import State

data OpAriBool = S | G | Se | Ge | E | NE

data UnOpBool = N

data BinOpBool = And | Or

opAriBoolTofunc :: OpAriBool -> (Int -> Int -> Bool)
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
