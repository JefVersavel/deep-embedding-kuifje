module TypeCheckers where

import Arithmetic
import Data.Either
import Data.Map hiding (foldr)
import State
import TypeCheck

typeCheckExpression :: UExpression -> Store -> String
typeCheckExpression exp store = fromLeft "" $ typecheck exp store

typeCheckStatement :: UStatement -> Store -> String
typeCheckStatement (UAssign _ e) = typeCheckExpression e

typeCheckUpdate :: UUpdateLanguage -> Store -> String
typeCheckUpdate (UURet stat) store = typeCheckStatement stat store
typeCheckUpdate (UUUni stats) store = foldr (\x y -> typeCheckStatement x store ++ "\n" ++ y) [] stats
typeCheckUpdate (UUChoose _ l r) store = typeCheckStatement l store ++ "\n" ++ typeCheckStatement r store
typeCheckUpdate (UUUniAssign _ e) store = typeCheckExpression e store

typeTest = typeCheckExpression (UIntCalc Add (ULit (5 :: Int)) (ULit (5 :: Int))) $ Store empty
