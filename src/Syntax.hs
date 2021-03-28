{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Syntax where

import Language
import Language.Kuifje.Distribution
import State

type a ~> b = a -> Dist b

data Bobby a where
  Skip :: Bobby a
  Update :: UpdateLanguage a -> Bobby a -> Bobby a
  If :: ConditionLanguage -> Bobby a -> Bobby a -> Bobby a -> Bobby a
  While :: ConditionLanguage -> Bobby a -> Bobby a -> Bobby a
  Observe :: ObserveLanguage a -> Bobby a -> Bobby a

instance Semigroup (Bobby a) where
  Skip <> k = k
  Update f p <> k = Update f (p <> k)
  While c p q <> k = While c p (q <> k)
  If c p q r <> k = If c p q (r <> k)
  Observe f p <> k = Observe f (p <> k)

-- | Return a 'Skip' instruction.
skip :: Bobby a
skip = Skip

-- | Return an 'Update' instruction.
update :: UpdateLanguage a -> Bobby a
update f = Update f skip

-- | Return a 'While' instruction.
while :: ConditionLanguage -> Bobby a -> Bobby a
while c p = While c p skip

-- | Return an 'If' instruction.
cond :: ConditionLanguage -> Bobby a -> Bobby a -> Bobby a
cond c p q = If c p q skip

-- | Return an 'Observe' instruction.
observe :: ObserveLanguage a -> Bobby a
observe o = Observe o skip
