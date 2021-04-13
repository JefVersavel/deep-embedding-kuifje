module Examples.USideChannel where

import Arithmetic
import Boolean
import Data.Map hiding (uupdate)
import Data.Maybe
import Language
import Language.Kuifje.Distribution
import Semantics
import State
import Syntax
import Type
import TypeCheck
import Prelude hiding (fmap, lookup)

initStore :: Int -> Int -> Store
initStore base exp = Store $ fromList [("base", I base), ("exp", I exp), ("e", I 0), ("d", I 0), ("p", I 0)]

exponentiation :: [Int] -> UBobby
exponentiation ds =
  uupdate (UURet $ UAssign "e" $ UVar "exp")
    <> uupdate (UURet $ UAssign "p" $ ULit (1 :: Int))
    <> uwhile
      (UCRet (UBoolCalc NE (UVar "e") (ULit (0 :: Int))))
      ( uupdate (UUUniAssign "d" (ULit ds))
          <> ucond
            (UCRet $ UBoolCalc NE (UIntCalc Mod (UVar "e") (UVar "d")) (ULit (0 :: Int)))
            ( uupdate
                ( UURet $
                    UAssign "p" $
                      UIntCalc
                        Power
                        (UIntCalc Mul (UVar "p") (UVar "base"))
                        (UIntCalc Mod (UVar "e") (UVar "d"))
                )
                <> uupdate
                  ( UURet $
                      UAssign "e" $
                        UIntCalc Sub (UVar "e") (UIntCalc Mod (UVar "e") (UVar "d"))
                  )
            )
            uskip
          <> uupdate (UURet $ UAssign "base" $ UIntCalc Power (UVar "base") (UVar "d"))
          <> uupdate (UURet $ UAssign "e" $ UIntCalc Div (UVar "e") (UVar "d"))
      )

sideProject :: Dist (Dist Store) -> Dist (Dist Literal)
sideProject = fmap (fmap (fromJust . lookup "exp" . runStore))

uhyper2 :: Dist (Dist Literal)
uhyper2 = sideProject $ uHysemBobby (exponentiation [2]) (uniform [initStore 6 exp | exp <- [0 .. 15]])

uhyper235 :: Dist (Dist Literal)
uhyper235 = sideProject $ uHysemBobby (exponentiation [2, 3, 5]) (uniform [initStore 6 exp | exp <- [0 .. 15]])
