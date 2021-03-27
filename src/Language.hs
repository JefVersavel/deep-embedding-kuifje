module Language where

import Expression
import Language.Kuifje.Distribution
import State
import Prelude hiding (return)

data UpdateLanguage a = URet (Statement a) | UUni [Statement a]

update :: Expressable a => UpdateLanguage a -> Store -> Dist Store
update (URet s) store = return $ execute s store
update (UUni l) store = uniform [execute s store | s <- l]

data ConditionLanguage = CRet (Expression Bool) | CUni [Expression Bool]

condition :: ConditionLanguage -> Store -> Dist Bool
condition (CRet e) store = return $ evaluate e store
condition (CUni l) store = uniform [evaluate e store | e <- l]

data ObserveLanguage a = ORet (Expression a) | OUni [Expression a]

observation :: Expressable a => ObserveLanguage a -> Store -> Dist a
observation (ORet e) store = return $ evaluate e store
observation (OUni l) store = uniform [evaluate e store | e <- l]
