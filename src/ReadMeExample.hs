module ReadMeExample where

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

initStore :: Int -> Store
initStore x = Store $ fromList [("x", I x), ("y", I 0)]

program :: Bobby
program =
  update (URet $ Assign "y" $ IntLit 0)
    <> while
      (CRet $ BoolCalc G (Var "x") (IntLit 0))
      ( update
          (URet $ Assign "y" $ IntCalc Add (Var "x") (Var "y"))
          <> update
            (URet $ Assign "x" $ IntCalc Sub (Var "x") (IntLit 1))
      )

project :: Dist (Dist Store) -> Dist (Dist Literal)
project = fmap (fmap (fromJust . lookup "y" . runStore))

hyper :: Dist (Dist Store)
hyper = hysemBobby program (uniform [initStore x | x <- [5 .. 8]])
