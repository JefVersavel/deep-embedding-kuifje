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

program :: Bobby Int
program =
  update (URet (Assign (IntLit 0) "y"))
    <> while
      (CRet (BoolCalc G (Var "x") (IntLit 0)))
      ( update
          ( URet (Assign (CharLit 'c') "y")
          )
          <> update
            ( URet (Assign (IntCalc Sub (Var "x") (IntLit 1)) "x")
            )
      )

project :: Dist (Dist Store) -> Dist (Dist Literal)
project = fmap (fmap (fromJust . lookup "y" . runStore))

hyper :: Dist (Dist Literal)
hyper = project $ hysemBobby program (uniform [initStore x | x <- [5 .. 8]])
