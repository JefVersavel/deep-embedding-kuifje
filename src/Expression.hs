{-# LANGUAGE TypeFamilies #-}

module Expression where

import Arithmetic
import Boolean
import Data.Map
import Language.Kuifje.Distribution
import State
import Prelude hiding (lookup, return)

data BinOp = A BinOpAri | B BinOpBool | BA OpAriBool

data UnOp = Bu UnOpBool

class (Ord a, ToLiteral a) => Expressable a where
  data Expression a :: *
  evaluate :: Expression a -> Store -> a

instance Expressable Int where
  data Expression Int
    = IntLit Int
    | IntVar String
    | IntCalc BinOpAri (Expression Int) (Expression Int)
  evaluate (IntLit i) _ = i
  evaluate (IntVar s) store = case lookup s store of
    Just (I i) -> i
    _ -> error "this is not good"
  evaluate (IntCalc o l r) store = opAriToFunc o left right
    where
      left = evaluate l store
      right = evaluate r store

instance Expressable Bool where
  data Expression Bool
    = BoolLit Bool
    | BoolVar String
    | BoolCalc OpAriBool (Expression Int) (Expression Int)
  evaluate (BoolCalc o l r) store =
    opAriBoolTofunc o (evaluate l store) (evaluate r store)

instance Expressable Char where
  data Expression Char = CharLit Char
  evaluate (CharLit c) _ = c

instance (Enum a, Expressable a) => Expressable [a] where
  data Expression [a] = ListLit [a] | Range a a
  evaluate (ListLit l) _ = l
  evaluate (Range l r) _ = [l .. r]

data Statement a = Assign String (Expression a)

execute :: Expressable a => Statement a -> Store -> Store
execute (Assign s e) store = insert s (toLiteral $ evaluate e store) store

assignTest = return $ execute (Assign "test" (IntLit 5)) empty
