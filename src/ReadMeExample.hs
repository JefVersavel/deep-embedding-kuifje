module ReadMeExample where

import Arithmetic
import Boolean
import Data.Map hiding (update)
import Data.Maybe
import Expression
import Language
import Language.Kuifje.Distribution
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax
import State
import Prelude hiding (fmap, lookup)

initStore :: Int -> Store
initStore x = Store $ fromList [("x", I x), ("y", I 0)]

program :: Kuifje Store
program =
  update (updateStatement (URet (Assign (IntLit 0) "y")))
    <> while
      (condition $ CRet (BoolCalc G (Var "x") (IntLit 0)))
      ( update
          ( updateStatement
              (URet (Assign (IntCalc Add (Var "x") (Var "y")) "y"))
          )
          <> update
            ( updateStatement
                (URet (Assign (IntCalc Sub (Var "x") (IntLit 1)) "x"))
            )
      )

project :: Dist (Dist Store) -> Dist (Dist Literal)
project = fmap (fmap (fromJust . lookup "y" . runStore))

hyper :: Dist (Dist Literal)
hyper = project $ hysem program (uniform [initStore x | x <- [5 .. 8]])
