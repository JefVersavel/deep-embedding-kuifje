module Examples.UPassword where

import Arithmetic
import Boolean
import Data.List (genericIndex, permutations, sortBy, (\\))
import Data.Map
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

makeState :: [Char] -> [Char] -> Store
makeState pw gs =
  Store $
    fromList
      [ ("pw", toLiteral pw),
        ("gs", toLiteral gs),
        ("l", L $ ILst []),
        ("i", I 0),
        ("ans", B True)
      ]

projectPw :: Dist (Dist Store) -> Dist (Dist Literal)
projectPw = fmap (fmap (fromJust . lookup "pw" . runStore))

initialDist :: String -> String -> Dist Store
initialDist pw gs = uniform [makeState pw' gs | pw' <- permutations pw]

basicI :: Int -> UBobby
basicI n =
  uupdate (UURet $ UAssign "n" $ ULit n)
    <> uupdate (UURet $ UAssign "i" $ ULit (0 :: Int))
    <> uupdate (UURet $ UAssign "ans" $ ULit True)
    <> uwhile
      (UCRet $ UBinBool And (UVar "ans") (UBoolCalc S (UVar "i") (UVar "n")))
      ( ucond
          (UCRet $ UBoolCalc NE (UElem (UVar "pw") (UVar "i")) (UElem (UVar "gs") (UVar "i")))
          ( uupdate (UURet $ UAssign "ans" $ ULit False)
          )
          uskip
          <> uupdate (UURet $ UAssign "i" $ UIntCalc Add (UVar "i") (ULit (1 :: Int)))
      )

uhyperI :: String -> String -> Dist (Dist Literal)
uhyperI pw gs = projectPw (uHysemBobby (basicI (length pw)) (initialDist pw gs))

basicL :: Int -> UBobby
basicL n =
  uupdate (UURet $ UAssign "n" $ ULit n)
    <> uupdate (UURet $ UAssign "i" $ ULit (0 :: Int))
    <> uupdate (UURet $ UAssign "ans" $ ULit True)
    <> uwhile
      (UCRet $ UBoolCalc S (UVar "i") (UVar "n"))
      ( ucond
          (UCRet $ UBoolCalc NE (UElem (UVar "pw") (UVar "i")) (UElem (UVar "gs") (UVar "i")))
          ( uupdate (UURet $ UAssign "ans" $ ULit False)
          )
          uskip
          <> uupdate (UURet $ UAssign "i" $ UIntCalc Add (UVar "i") (ULit (1 :: Int)))
      )

uhyperL :: String -> String -> Dist (Dist Literal)
uhyperL pw gs = projectPw (uHysemBobby (basicL (length pw)) (initialDist pw gs))

basicM :: Int -> UBobby
basicM n =
  uupdate (UURet $ UAssign "n" $ ULit n)
    <> uupdate (UURet $ UAssign "i" $ ULit (0 :: Int))
    <> uupdate (UURet $ UAssign "ans" $ ULit True)
    <> uwhile
      (UCRet $ UBoolCalc S (UVar "i") (UVar "n"))
      ( uupdate
          ( UURet $
              UAssign "ans" $
                UBinBool
                  And
                  (UVar "ans")
                  ( UBoolCalc
                      E
                      (UElem (UVar "pw") (UVar "i"))
                      (UElem (UVar "gs") (UVar "i"))
                  )
          )
          <> uupdate (UURet $ UAssign "i" $ UIntCalc Add (UVar "i") (ULit (1 :: Int)))
      )

uhyperM :: String -> String -> Dist (Dist Literal)
uhyperM pw gs = projectPw (uHysemBobby (basicM (length pw)) (initialDist pw gs))

basicN :: Int -> UBobby
basicN n =
  uupdate (UURet $ UAssign "n" $ ULit n)
    <> uupdate (UURet $ UAssign "i" $ ULit (0 :: Int))
    <> uupdate (UURet $ UAssign "ans" $ ULit True)
    <> uwhile
      (UCRet $ UBoolCalc S (UVar "i") (UVar "n"))
      ( uupdate
          ( UURet $
              UAssign "ans" $
                UBinBool
                  And
                  (UVar "ans")
                  ( UBoolCalc
                      E
                      (UElem (UVar "pw") (UVar "i"))
                      (UElem (UVar "gs") (UVar "i"))
                  )
          )
          <> uupdate (UURet $ UAssign "i" $ UIntCalc Add (UVar "i") (ULit (1 :: Int)))
      )
    <> uobserve (UORet (UVar "ans"))

uhyperN :: String -> String -> Dist (Dist Literal)
uhyperN pw gs = projectPw (uHysemBobby (basicN (length pw)) (initialDist pw gs))

basicR :: Int -> UBobby
basicR n =
  uupdate (UURet $ UAssign "n" $ ULit n)
    <> uupdate (UURet $ UAssign "l" $ URange (ULit (0 :: Int)) (UIntCalc Sub (UVar "n") (ULit (1 :: Int))))
    <> uupdate (UURet $ UAssign "i" $ ULit (0 :: Int))
    <> uupdate (UURet $ UAssign "ans" $ ULit True)
    <> uwhile
      (UCRet $ UBinBool And (UVar "ans") (UBoolCalc NE (UVar "l") (ULit ([] :: [Int]))))
      ( uupdate
          (UUUniAssign "i" (UVar "l"))
          <> uupdate
            ( UURet $
                UAssign "ans" $
                  UBinBool
                    And
                    (UVar "ans")
                    ( UBoolCalc
                        E
                        (UElem (UVar "pw") (UVar "i"))
                        (UElem (UVar "gs") (UVar "i"))
                    )
            )
          <> uupdate (UURet $ UAssign "l" $ UListCalc LDiv (UVar "l") (UToList [UVar "i"]))
      )
    <> uobserve (UORet (UVar "ans"))

uhyperR :: String -> String -> Dist (Dist Literal)
uhyperR pw gs = projectPw (uHysemBobby (basicR (length pw)) (initialDist pw gs))

basicS :: Int -> UBobby
basicS n =
  uupdate (UURet $ UAssign "n" $ ULit n)
    <> uupdate (UURet $ UAssign "l" $ URange (ULit (0 :: Int)) (UIntCalc Sub (UVar "n") (ULit (1 :: Int))))
    <> uupdate (UURet $ UAssign "i" $ ULit (0 :: Int))
    <> uupdate (UURet $ UAssign "ans" $ ULit True)
    <> uwhile
      (UCRet $ UBinBool And (UVar "ans") (UBoolCalc NE (UVar "l") (ULit ([] :: [Int]))))
      ( uupdate
          (UUUniAssign "i" (UVar "l"))
          <> ucond
            ( UCRet $
                UBoolCalc
                  NE
                  (UElem (UVar "pw") (UVar "i"))
                  (UElem (UVar "gs") (UVar "i"))
            )
            (uupdate (UURet $ UAssign "ans" $ ULit False))
            uskip
          <> uupdate (UURet $ UAssign "l" $ UListCalc LDiv (UVar "l") (UToList [UVar "i"]))
      )
    <> uobserve (UORet (UVar "ans"))

uhyperS :: String -> String -> Dist (Dist Literal)
uhyperS pw gs = projectPw (uHysemBobby (basicS (length pw)) (initialDist pw gs))
