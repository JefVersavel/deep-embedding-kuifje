{-# LANGUAGE GADTs #-}

module Expression where

import Arithmetic
import Boolean
import Data.Map hiding (map)
import Language.Kuifje.Distribution hiding (map)
import State
import Prelude hiding (lookup, return)

data Expression a where
  Var :: String -> Expression a
  IntLit :: Int -> Expression Int
  BoolLit :: Bool -> Expression Bool
  CharLit :: Char -> Expression Char
  ListLit :: [a] -> Expression [a]
  IntCalc :: BinOpAri -> Expression Int -> Expression Int -> Expression Int
  BoolCalc :: OpAriBool -> Expression Int -> Expression Int -> Expression Bool
  BinBool :: BinOpBool -> Expression Bool -> Expression Bool -> Expression Bool
  UniBool :: UnOpBool -> Expression Bool -> Expression Bool
  Range :: (ToType a, Enum a) => Expression a -> Expression a -> Expression [a]
  Elem :: Expression [a] -> Expression Int -> Expression a

eval :: ToType a => Expression a -> Store -> a
eval (Var s) store = case lookup s (runStore store) of
  Just lit -> toType lit
  Nothing -> error $ "variable " ++ s ++ " was not found"
eval (IntLit i) _ = i
eval (BoolLit b) _ = b
eval (CharLit c) _ = c
eval (ListLit l) _ = l
eval (IntCalc o l r) store = opAriToFunc o (eval l store) (eval r store)
eval (BoolCalc o l r) store = opAriBoolTofunc o (eval l store) (eval r store)
eval (BinBool o l r) store = binOpBoolToFunc o (eval l store) (eval r store)
eval (UniBool o b) store = unOpBoolToFunc o $ eval b store
eval (Range l r) store = [eval l store .. eval r store]
eval (Elem l i) store = eval l store !! eval i store

data Statement where
  AssignInt :: Expression Int -> String -> Statement
  AssignBool :: Expression Bool -> String -> Statement
  AssignChar :: Expression Char -> String -> Statement
  AssignList :: (ToType a, ToLiteral a) => Expression [a] -> String -> Statement

execute :: Statement -> Store -> Store
execute (AssignInt e s) store =
  Store $
    insert s (toLiteral $ eval e store) (runStore store)
execute (AssignBool e s) store =
  Store $
    insert s (toLiteral $ eval e store) (runStore store)
execute (AssignChar e s) store =
  Store $
    insert s (toLiteral $ eval e store) (runStore store)
execute (AssignList e s) store =
  Store $
    insert s (toLiteral $ eval e store) (runStore store)

testExp = Elem (ListLit ['a', 'c']) (Var "test")

testStore = Store $ fromList [("test", I 0), ("sec", I 6)]

testEval = eval testExp testStore
