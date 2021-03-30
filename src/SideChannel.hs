module SideChannel where

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
import Prelude hiding (fmap, lookup)

initStore :: Int -> Int -> Store
initStore base exp = Store $ fromList [("base", I base), ("exp", I exp), ("e", I 0), ("d", I 0), ("p", I 0)]

exponentiation :: [Int] -> Bobby
exponentiation ds =
  update (URet (AssignInt (Var "exp") "e"))
    <> update (URet (AssignInt (IntLit 1) "p"))
    <> while
      (CRet (BoolCalc NE (Var "e") (IntLit 0)))
      ( update (UUniAssignInt "d" (ListLit ds))
          <> update (URet $ AssignInt (IntLit 0) "e")
          <> cond
            (CRet (BoolCalc NE (IntCalc Mod (Var "e") (Var "d")) (IntLit 0)))
            skip
            skip
      )

expon :: Dist (Dist Store)
expon = hysemBobby (exponentiation [2, 3, 4]) $ uniform [initStore 6 exp | exp <- [0 .. 15]]
