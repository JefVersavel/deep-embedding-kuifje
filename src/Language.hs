{-# LANGUAGE ExistentialQuantification #-}

module Language where

import Expression
import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint
import State
import Prelude hiding (return)

data UpdateLanguage
  = URet (Statement)
  | UUni [Statement]
  | UChoose Prob (Statement) (Statement)

updateStatement :: UpdateLanguage -> Store -> Dist Store
updateStatement (URet s) store = return $ execute s store
updateStatement (UUni l) store = uniform [execute s store | s <- l]
updateStatement (UChoose p l r) store = choose p (execute l store) (execute r store)

data ConditionLanguage
  = CRet (Expression Bool)
  | CUni [Expression Bool]
  | CChoose Prob (Expression Bool) (Expression Bool)

condition :: ConditionLanguage -> Store -> Dist Bool
condition (CRet e) store = return $ eval e store
condition (CUni l) store = uniform [eval e store | e <- l]
condition (CChoose p l r) store = choose p (eval l store) (eval r store)

data Observation
  = IntOb (Expression Int)
  | BoolOb (Expression Bool)
  | CharOb (Expression Char)
  | forall a. (ToLiteral a, ToType a) => LitsOb (Expression [a])

data ObserveLanguage
  = ORet Observation
  | OUni Observation
  | OChoose Prob Observation Observation

toExpr (IntOb e) = e

observation :: (ToType a, Ord a) => ObserveLanguage -> Store -> Dist a
observation (ORet e) store = return $ eval (toExpr e) store
observation (OUni l) store = uniform [eval e store | e <- l]
observation (OChoose p l r) store = choose p (eval l store) (eval r store)

testje =
  print $
    updateStatement
      (UUni [AssignChar testExp "alright", AssignChar testExp "not good"])
      testStore

--
-- data Kuifje s a
--   = Skip
--   | Update (UpdateLanguage a) (Kuifje s a)
--   | If (ConditionLanguage) (Kuifje s a) (Kuifje s a) (Kuifje s a)
--   | While (ConditionLanguage) (Kuifje s a) (Kuifje s a)
--   | Observe (ObserveLanguage a) (Kuifje s a)
