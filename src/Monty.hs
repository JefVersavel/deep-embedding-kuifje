module Monty where

import Arithmetic
import Boolean
import Data.Map hiding (update)
import Data.Maybe
import Expression
import Language
import Language.Kuifje.Distribution
import Semantics
import State
import Syntax
import Type
import Prelude hiding (fmap, lookup)

initStore :: Char -> Store
initStore c = Store $ fromList [("door", toLiteral c)]

hall :: Char -> Bobby
hall chosenDoor = observe (OUni CType $ ListDiv (Lit "abc") (ToList [Var "door", Lit chosenDoor]))

doors :: Dist Store
doors = uniform [initStore 'a', initStore 'b', initStore 'c']

monty :: Dist (Dist Store)
monty = hysemBobby (hall 'a') doors
