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
  update (URet $ Assign "e" $ IntVar "exp")
    <> update (URet $ Assign "p" $ IntLit 1)
    <> while
      (CRet (BoolCalc NE (Var "e") (IntLit 0)))
      ( update (UUniAssignInt "d" (ListLit ds))
          <> cond
            (CRet $ BoolCalc NE (IntCalc Mod (Var "e") (Var "d")) (IntLit 0))
            ( update
                ( URet $
                    Assign "p" $
                      IntCalc
                        Power
                        (IntCalc Mul (Var "p") (Var "base"))
                        (IntCalc Mod (Var "e") (Var "d"))
                )
                <> update
                  ( URet $
                      Assign "e" $
                        IntCalc Sub (Var "e") (IntCalc Mod (Var "e") (Var "d"))
                  )
            )
            skip
          <> update (URet $ Assign "base" $ IntCalc Power (Var "base") (Var "d"))
          <> update (URet $ Assign "e" $ IntCalc Div (Var "e") (Var "d"))
      )

sideProject :: Dist (Dist Store) -> Dist (Dist Literal)
sideProject = fmap (fmap (fromJust . lookup "exp" . runStore))

hyper2 :: Dist (Dist Literal)
hyper2 = sideProject $ hysemBobby (exponentiation [2]) (uniform [initStore 6 exp | exp <- [0 .. 15]])

hyper235 :: Dist (Dist Literal)
hyper235 = sideProject $ hysemBobby (exponentiation [2, 3, 5]) (uniform [initStore 6 exp | exp <- [0 .. 15]])
