{-# LANGUAGE TypeFamilies #-}

module Expression where

import Arithmetic
import Boolean
import Data.Map hiding (map)
import Language.Kuifje.Distribution hiding (map)
import State
import Prelude hiding (lookup, return)

class (Ord a, ToLiteral a) => Expressable a where
  data Expression a :: *
  evaluate :: Expression a -> Store -> Literal

instance Expressable Int where
  data Expression Int
    = IntLit Int
    | IntVar String
    | IntCalc BinOpAri (Expression Int) (Expression Int)
  evaluate (IntLit i) _ = I i
  evaluate (IntVar s) store = case lookup s store of
    Just (I i) -> I i
    _ -> error "this is not good"
  evaluate (IntCalc o l r) store = I $ opAriToFunc o left right
    where
      left = fromLiteral $ evaluate l store
      right = fromLiteral $ evaluate r store

instance Expressable Bool where
  data Expression Bool
    = BoolLit Bool
    | BoolVar String
    | BoolCalc OpAriBool (Expression Int) (Expression Int)
  evaluate (BoolCalc o l r) store =
    B $
      opAriBoolTofunc
        o
        (fromLiteral $ evaluate l store)
        (fromLiteral $ evaluate r store)

instance Expressable Char where
  data Expression Char = CharLit Char
  evaluate (CharLit c) _ = C c

instance (Enum a, Expressable a) => Expressable [a] where
  data Expression [a] = ListLit [a] | Range a a | Elem [a] (Expression Int)
  evaluate (ListLit l) _ = toLiteral l
  evaluate (Range l r) _ = L [toLiteral i | i <- [l .. r]]
  evaluate (Elem l e) store =
    map toLiteral l
      !! fromLiteral (evaluate e store)

data Statement a = Assign String (Expression a)

execute :: Expressable a => Statement a -> Store -> Store
execute (Assign s e) store = insert s (evaluate e store) store

assignTest = return $ execute (Assign "test" (IntLit 5)) empty
