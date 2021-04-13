{-# LANGUAGE GADTs #-}

module Expression where

import Arithmetic
import Boolean
import CharComparison
import Data.List ((\\))
import Data.Map hiding (map, (\\))
import Debug.Trace
import Language.Kuifje.Distribution hiding (map)
import ListComparison
import State
import Type
import Prelude hiding (lookup, return)

data Expression a where
  Var :: String -> Expression a
  Lit :: (ToType a, Show a) => a -> Expression a
  IntCalc :: BinOpAri -> Expression Int -> Expression Int -> Expression Int
  BoolCalc :: (Ord a, ToType a, Show a) => Type a -> OpAriBool -> Expression a -> Expression a -> Expression Bool
  BinBool :: BinOpBool -> Expression Bool -> Expression Bool -> Expression Bool
  UniBool :: UnOpBool -> Expression Bool -> Expression Bool
  Range :: (ToType a, Enum a, Show a) => Expression a -> Expression a -> Expression [a]
  Elem :: (ToType a, Show a) => Expression [a] -> Expression Int -> Expression a
  ListDiv :: (ToType a, Eq a, Show a) => Expression [a] -> Expression [a] -> Expression [a]
  Singleton :: (ToType a, Show a) => Expression a -> Expression [a]
  Empty :: (ToType a, Show a) => Type a -> Expression [a]
  ToList :: (ToType a, Show a) => [Expression a] -> Expression [a]

instance Show (Expression a) where
  show (Var s) = s
  show (Lit a) = show a
  show (IntCalc o l r) = show l ++ " " ++ show o ++ " " ++ show r
  show (BoolCalc _ o l r) = show l ++ " " ++ show o ++ " " ++ show r
  show (BinBool o l r) = show l ++ " " ++ show o ++ " " ++ show r
  show (UniBool o e) = show o ++ " " ++ show e
  show (Range b e) = "[" ++ show b ++ " .. " ++ show e ++ "]"
  show (Elem l i) = show l ++ "!!" ++ show i
  show (ListDiv l r) = show l ++ "\\" ++ show r
  show (Singleton e) = "[" ++ show e ++ "]"
  show (Empty _) = "[]"
  show (ToList l) = show l

eval :: ToType a => Expression a -> Store -> a
eval (Var s) store = case lookup s (runStore store) of
  Just lit -> toType lit
  Nothing -> error $ "variable " ++ s ++ " was not found"
eval (Lit i) _ = i
eval (IntCalc o l r) store = opAriToFunc o (eval l store) (eval r store)
eval (BoolCalc t o l r) store = opAriBoolTofunc o (eval l store) (eval r store)
eval (BinBool o l r) store = binOpBoolToFunc o (eval l store) (eval r store)
eval (UniBool o b) store = unOpBoolToFunc o $ eval b store
eval (Range l r) store = [eval l store .. eval r store]
eval (Elem l i) store = eval l store !! eval i store
eval (ListDiv l r) store = eval l store \\ eval r store
eval (Singleton e) store = [eval e store]
eval (Empty t) _ = []
eval (ToList l) store = (`eval` store) <$> l

data Expression' a where
  Lit' :: a -> Expression' a
  Add' :: Expression' Int -> Expression' Int -> Expression' Int
  Equal :: Expression' Int -> Expression' Int -> Expression' Bool

data Statement
  = forall a. (ToType a) => Assign String (Expression a)
  | forall a. ToType a => AssignAn (Type a) String (Expression a)
  | forall a. ToType a => (:=) (Type a, String) (Expression a)

execute :: Statement -> Store -> Store
execute (Assign s e) store =
  Store $
    insert s (toLiteral $ eval e store) (runStore store)
execute (AssignAn t s e) store =
  Store $ insert s (toLiteral $ eval e store) (runStore store)
execute ((t, s) := e) store =
  Store $ insert s (toLiteral $ eval e store) (runStore store)

testExp i = Lit (i :: Int)

testStore = Store $ fromList [("test", I 0), ("sec", I 6)]

testEval = eval (testExp 2) testStore
