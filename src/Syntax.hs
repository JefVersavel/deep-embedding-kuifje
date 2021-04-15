module Syntax where

import Language
import Language.Kuifje.Distribution
import State
import TypeCheck

data Bobby
  = Skip
  | Update UpdateLanguage Bobby
  | If ConditionLanguage Bobby Bobby Bobby
  | While ConditionLanguage Bobby Bobby
  | Observe ObserveLanguage' Bobby

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
observe :: ObserveLanguage' -> Bobby
observe o = Observe o skip

data UBobby
  = USkip
  | UUpdate UUpdateLanguage UBobby
  | UIf UConditionLanguage UBobby UBobby UBobby
  | UWhile UConditionLanguage UBobby UBobby
  | UObserve UObserveLanguage UBobby
  deriving (Show)

instance Semigroup UBobby where
  USkip <> k = k
  UUpdate f p <> k = UUpdate f (p <> k)
  UWhile c p q <> k = UWhile c p (q <> k)
  UIf c p q r <> k = UIf c p q (r <> k)
  UObserve f p <> k = UObserve f (p <> k)

-- | Return a 'Skip' instruction.
uskip :: UBobby
uskip = USkip

-- | Return an 'Update' instruction.
uupdate :: UUpdateLanguage -> UBobby
uupdate f = UUpdate f uskip

-- | Return a 'While' instruction.
uwhile :: UConditionLanguage -> UBobby -> UBobby
uwhile c p = UWhile c p uskip

-- | Return an 'If' instruction.
ucond :: UConditionLanguage -> UBobby -> UBobby -> UBobby
ucond c p q = UIf c p q uskip

-- | Return an 'Observe' instruction.
uobserve :: UObserveLanguage -> UBobby
uobserve o = UObserve o uskip
