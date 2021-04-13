module Examples.UReadMeExample where

import Arithmetic
import Boolean
import Data.Map hiding (update)
import Data.Maybe
import Language
import Language.Kuifje.Distribution
import Semantics
import State
import Syntax
import Type
import TypeCheck
import Prelude hiding (fmap, lookup)

initStore :: Int -> Store
initStore x = Store $ fromList [("x", I x), ("y", I 0)]

program :: UBobby
program =
  uupdate (UURet $ UAssign "y" $ ULit (0 :: Int))
    <> uwhile
      (UCRet $ UBoolCalc G (UVar "x") (ULit (0 :: Int)))
      ( uupdate
          (UURet $ UAssign "y" $ UIntCalc Add (UVar "x") (UVar "y"))
          <> uupdate
            (UURet $ UAssign "x" $ UIntCalc Sub (UVar "x") (ULit (1 :: Int)))
      )

project :: Dist (Dist Store) -> Dist (Dist Literal)
project = fmap (fmap (fromJust . lookup "y" . runStore))

uhyper :: Dist (Dist Literal)
uhyper = project $ uHysemBobby program (uniform [initStore x | x <- [5 .. 8]])
