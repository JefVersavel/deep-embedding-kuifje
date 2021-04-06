{-# LANGUAGE GADTs #-}

module Expression where

import Arithmetic
import Boolean
import CharComparison
import Data.Map hiding (map)
import Debug.Trace
import Language.Kuifje.Distribution hiding (map)
import ListComparison
import State
import Prelude hiding (lookup, return)

data Expression a where
  Var :: String -> Expression a
  IntVar :: String -> Expression Int
  BoolVar :: String -> Expression Bool
  CharVar :: String -> Expression Char
  ListVar :: (ToType a, ToLiteral a) => String -> Expression [a]
  IntLit :: Int -> Expression Int
  BoolLit :: Bool -> Expression Bool
  CharLit :: Char -> Expression Char
  ListLit :: [a] -> Expression [a]
  IntCalc :: BinOpAri -> Expression Int -> Expression Int -> Expression Int
  BoolCalc :: OpAriBool -> Expression Int -> Expression Int -> Expression Bool
  BinBool :: BinOpBool -> Expression Bool -> Expression Bool -> Expression Bool
  UniBool :: UnOpBool -> Expression Bool -> Expression Bool
  Range :: (ToType a, Enum a) => Expression a -> Expression a -> Expression [a]
  Elem :: (ToType a, Enum a, Show a) => Expression [a] -> Expression Int -> Expression a
  ListBool :: (Eq a, ToType a) => ListOp -> Expression [a] -> Expression [a] -> Expression Bool
  CharBool :: CharOp -> Expression Char -> Expression Char -> Expression Bool

eval :: ToType a => Expression a -> Store -> a
eval (Var s) store = case lookup s (runStore store) of
  Just lit -> toType lit
  Nothing -> error $ "variable " ++ s ++ " was not found"
eval (IntVar s) store = case lookup s (runStore store) of
  Just lit -> toType lit
  Nothing -> error $ "variable " ++ s ++ " was not found"
eval (BoolVar s) store = case lookup s (runStore store) of
  Just lit -> toType lit
  Nothing -> error $ "variable " ++ s ++ " was not found"
eval (CharVar s) store = case lookup s (runStore store) of
  Just lit -> toType lit
  Nothing -> error $ "variable " ++ s ++ " was not found"
eval (ListVar s) store = case lookup s (runStore store) of
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
eval (ListBool o l r) store = listOpToFunc o (eval l store) (eval r store)
eval (CharBool o l r) store = charOpToFunc o (eval l store) (eval r store)

data Statement where
  Assign :: (ToType a, ToLiteral a) => String -> Expression a -> Statement

execute (Assign s e) store =
  Store $
    insert s (toLiteral $ eval e store) (runStore store)

testExp = Elem (ListLit ['a', 'c']) (Var "test")

testStore = Store $ fromList [("test", I 0), ("sec", I 6)]

testEval = eval testExp testStore
