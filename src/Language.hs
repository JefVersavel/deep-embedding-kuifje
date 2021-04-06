{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Language where

import Expression
import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint
import State
import Prelude hiding (return)

data UpdateLanguage
  = URet Statement
  | UUni [Statement]
  | UChoose Prob Statement Statement
  | UUniAssignInt String (Expression [Int])

updateStatement :: UpdateLanguage -> Store -> Dist Store
updateStatement (URet s) store = return $ execute s store
updateStatement (UUni l) store = uniform [execute s store | s <- l]
updateStatement (UChoose p l r) store = choose p (execute l store) (execute r store)
updateStatement (UUniAssignInt s e) store = uniform [execute (Assign s (IntLit ev)) store | ev <- eval e store]

data ConditionLanguage
  = CRet (Expression Bool)
  | CUni [Expression Bool]
  | CChoose Prob (Expression Bool) (Expression Bool)

condition :: ConditionLanguage -> Store -> Dist Bool
condition (CRet e) store = return $ eval e store
condition (CUni l) store = uniform [eval e store | e <- l]
condition (CChoose p l r) store = choose p (eval l store) (eval r store)

data ObserveLanguage a where
  ORet :: Expression a -> ObserveLanguage a
  OUni :: [Expression a] -> ObserveLanguage a
  OChoose :: Prob -> Expression a -> Expression a -> ObserveLanguage a

observation :: (ToType a, Ord a) => ObserveLanguage a -> Store -> Dist a
observation (ORet e) store = return $ eval e store
observation (OUni l) store = uniform [eval e store | e <- l]
observation (OChoose p l r) store = choose p (eval l store) (eval r store)

testje =
  print $
    updateStatement
      (UUni [Assign "alright" testExp, Assign "not good" testExp])
      testStore

--
-- data Kuifje s a
--   = Skip
--   | Update (UpdateLanguage a) (Kuifje s a)
--   | If (ConditionLanguage) (Kuifje s a) (Kuifje s a) (Kuifje s a)
--   | While (ConditionLanguage) (Kuifje s a) (Kuifje s a)
--   | Observe (ObserveLanguage a) (Kuifje s a)
