module Password where

import Arithmetic
import Boolean
import CharComparison
import Data.List (genericIndex, permutations, sortBy, (\\))
import Data.Map hiding (update)
import Data.Maybe
import Expression
import Language
import Language.Kuifje.Distribution
import Semantics
import State
import Syntax
import Prelude hiding (fmap, lookup)

makeState :: [Char] -> [Char] -> Store
makeState pw gs =
  Store $
    fromList
      [ ("pw", toLiteral pw),
        ("gs", toLiteral gs),
        ("l", L []),
        ("i", I 0),
        ("ans", B True)
      ]

projectPw :: Dist (Dist Store) -> Dist (Dist Literal)
projectPw = fmap (fmap (fromJust . lookup "pw" . runStore))

initialDist :: String -> String -> Dist Store
initialDist pw gs = uniform [makeState pw' gs | pw' <- permutations pw]

basicI :: Int -> Bobby
basicI n =
  update (URet $ Assign "n" $ IntLit n)
    <> update (URet $ Assign "i" $ IntLit 0)
    <> update (URet $ Assign "ans" $ BoolLit True)
    <> while
      (CRet $ BinBool And (Var "ans") (BoolCalc S (Var "i") (Var "n")))
      ( cond
          (CRet $ CharBool NEc (Elem (Var "pw") (Var "i")) (Elem (Var "gs") (Var "i")))
          ( update (URet $ Assign "ans" $ BoolLit False)
          )
          skip
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (IntLit 1))
      )

hyperI :: String -> String -> Dist (Dist Literal)
hyperI pw gs = projectPw (hysemBobby (basicI (length pw)) (initialDist pw gs))

basicL :: Int -> Bobby
basicL n =
  update (URet $ Assign "n" $ IntLit n)
    <> update (URet $ Assign "i" $ IntLit 0)
    <> update (URet $ Assign "ans" $ BoolLit True)
    <> while
      (CRet $ BoolCalc S (Var "i") (Var "n"))
      ( cond
          (CRet $ CharBool NEc (Elem (Var "pw") (Var "i")) (Elem (Var "gs") (Var "i")))
          ( update (URet $ Assign "ans" $ BoolLit False)
          )
          skip
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (IntLit 1))
      )

hyperL :: String -> String -> Dist (Dist Literal)
hyperL pw gs = projectPw (hysemBobby (basicL (length pw)) (initialDist pw gs))

basicM :: Int -> Bobby
basicM n =
  update (URet $ Assign "n" $ IntLit n)
    <> update (URet $ Assign "i" $ IntLit 0)
    <> update (URet $ Assign "ans" $ BoolLit True)
    <> while
      (CRet $ BoolCalc S (Var "i") (Var "n"))
      ( update
          ( URet $
              Assign "ans" $
                BinBool
                  And
                  (Var "ans")
                  ( CharBool
                      Ec
                      (Elem (Var "pw") (Var "i"))
                      (Elem (Var "gs") (Var "i"))
                  )
          )
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (IntLit 1))
      )

hyperM :: String -> String -> Dist (Dist Literal)
hyperM pw gs = projectPw (hysemBobby (basicM (length pw)) (initialDist pw gs))

basicN :: Int -> Bobby
basicN n =
  update (URet $ Assign "n" $ IntLit n)
    <> update (URet $ Assign "i" $ IntLit 0)
    <> update (URet $ Assign "ans" $ BoolLit True)
    <> while
      (CRet $ BoolCalc S (Var "i") (Var "n"))
      ( update
          ( URet $
              Assign "ans" $
                BinBool
                  And
                  (Var "ans")
                  ( CharBool
                      Ec
                      (Elem (Var "pw") (Var "i"))
                      (Elem (Var "gs") (Var "i"))
                  )
          )
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (IntLit 1))
      )
    <> observe (ORet BType (Var "ans"))

hyperN :: String -> String -> Dist (Dist Literal)
hyperN pw gs = projectPw (hysemBobby (basicN (length pw)) (initialDist pw gs))
