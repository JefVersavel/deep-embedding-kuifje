module TypeCheckers where

import Arithmetic
import Data.Either
import Data.Map hiding (foldr)
import State
import Syntax
import TypeCheck

typeCheckExpression :: UExpression -> Store -> String
typeCheckExpression exp store = fromLeft "" $ typecheck exp store

typeCheckStatement :: UStatement -> Store -> String
typeCheckStatement (UAssign _ e) = typeCheckExpression e

typeCheckUpdate :: UUpdateLanguage -> Store -> String
typeCheckUpdate (UURet stat) store = typeCheckStatement stat store
typeCheckUpdate (UUUni stats) store =
  foldr
    (\x y -> typeCheckStatement x store ++ " " ++ y)
    []
    stats
typeCheckUpdate (UUChoose _ l r) store =
  typeCheckStatement l store
    ++ " "
    ++ typeCheckStatement r store
typeCheckUpdate (UUUniAssign _ e) store = typeCheckExpression e store

typeCheckCondition :: UConditionLanguage -> Store -> String
typeCheckCondition (UCRet e) store = typeCheckExpression e store
typeCheckCondition (UCUni exps) store =
  foldr
    (\x y -> typeCheckExpression x store ++ " " ++ y)
    []
    exps
typeCheckCondition (UCChoose _ l r) store =
  typeCheckExpression l store
    ++ " "
    ++ typeCheckExpression r store

typeCheckObservation :: UObserveLanguage -> Store -> String
typeCheckObservation (UORet e) store = typeCheckExpression e store
typeCheckObservation (UOUni e) store = typeCheckExpression e store
typeCheckObservation (UOChoose _ l r) store =
  typeCheckExpression l store
    ++ " "
    ++ typeCheckExpression r store

typeCheckBobby :: UBobby -> Store -> String
typeCheckBobby USkip store = ""
typeCheckBobby (UUpdate f p) store =
  typeCheckUpdate f store
    ++ " "
    ++ typeCheckBobby p store
typeCheckBobby (UIf c p q r) store =
  typeCheckCondition c store
    ++ " "
    ++ typeCheckBobby p store
    ++ " "
    ++ typeCheckBobby q store
    ++ " "
    ++ typeCheckBobby r store
typeCheckBobby (UWhile c p q) store =
  typeCheckCondition c store
    ++ " "
    ++ typeCheckBobby p store
    ++ " "
    ++ typeCheckBobby q store
typeCheckBobby (UObserve f p) store =
  typeCheckObservation f store
    ++ " "
    ++ typeCheckBobby p store

typeCheckProgram :: UBobby -> Store -> String
typeCheckProgram program store
  | allSpaces str = "your code is correctly typed"
  | otherwise = str
  where
    str = typeCheckBobby program store

allSpaces [] = True
allSpaces (x : xs)
  | x == ' ' = allSpaces xs
  | otherwise = False

typeTest = typeCheckExpression (UIntCalc Add (ULit (5 :: Int)) (ULit (5 :: Int))) $ Store empty
