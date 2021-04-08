{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Syntax where

import Language
import Language.Kuifje.Distribution
import State

type a ~> b = a -> Dist b

data Bobby where
  Skip :: Bobby
  Update :: UpdateLanguage -> Bobby -> Bobby
  If :: ConditionLanguage -> Bobby -> Bobby -> Bobby -> Bobby
  While :: ConditionLanguage -> Bobby -> Bobby -> Bobby
  Observe :: (Ord a, ToType a) => ObserveLanguage a -> Bobby -> Bobby

instance Semigroup Bobby where
  Skip <> k = k
  Update f p <> k = Update f (p <> k)
  While c p q <> k = While c p (q <> k)
  If c p q r <> k = If c p q (r <> k)
  Observe f p <> k = Observe f (p <> k)

-- | Return a 'Skip' instruction.
skip :: Bobby
skip = Skip

-- | Return an 'Update' instruction.
update :: UpdateLanguage -> Bobby
update f = Update f skip

-- | Return a 'While' instruction.
while :: ConditionLanguage -> Bobby -> Bobby
while c p = While c p skip

-- | Return an 'If' instruction.
cond :: ConditionLanguage -> Bobby -> Bobby -> Bobby
cond c p q = If c p q skip

-- | Return an 'Observe' instruction.
observe :: (Ord a, ToType a) => ObserveLanguage a -> Bobby
observe o = Observe o skip
