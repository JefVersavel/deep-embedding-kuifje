module Examples.ReadMeExampleKuifje where

import Arithmetic
import Boolean
import Data.Map hiding (update)
import Data.Maybe
import Expression
import Language.Kuifje.Distribution
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax
import State
import Type
import Prelude hiding (fmap, lookup, return)

initStore :: Int -> Store
initStore x = Store $ fromList [("x", I x), ("y", I 0)]

program :: Kuifje Store
program =
  update (return . execute ((IType, "y") := Lit 0))
    <> while
      (return . eval (BoolCalc IType G (Var "x") (Lit 0)))
      ( update
          (return . execute (Assign "y" (IntCalc Add (Var "x") (Var "y"))))
          <> update
            (return . execute (Assign "x" (IntCalc Sub (Var "x") (Lit 1))))
      )

project :: Dist (Dist Store) -> Dist (Dist Literal)
project = fmap (fmap (fromJust . lookup "y" . runStore))

hyperKuifje :: Dist (Dist Literal)
hyperKuifje = project $ hysem program (uniform [initStore x | x <- [5 .. 8]])
