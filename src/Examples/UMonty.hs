module Examples.UMonty where

import Arithmetic
import Boolean
import Data.Map hiding (update)
import Data.Maybe
import Language
import Language.Kuifje.Distribution
import ListCalculations
import Semantics
import State
import Syntax
import Type
import TypeCheck
import Prelude hiding (fmap, lookup)

initStore :: Char -> Store
initStore c = Store $ fromList [("door", toLiteral c)]

hall :: Char -> UBobby
hall chosenDoor = uobserve (UOUni $ UListCalc LDiv (ULit "abc") (UToList [UVar "door", ULit chosenDoor]))

doors :: Dist Store
doors = uniform [initStore 'a', initStore 'b', initStore 'c']

umonty :: Dist (Dist Store)
umonty = uHysemBobby (hall 'a') doors
