module Examples.Password where

import Arithmetic
import Boolean
import CharComparison
import Data.List (genericIndex, permutations, sortBy, (\\))
import Data.Map hiding (update)
import Data.Maybe
import Expression
import Language
import Language.Kuifje.Distribution
import ListComparison
import Semantics
import State
import Syntax
import Type
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
  update (URet $ Assign "n" $ Lit n)
    <> update (URet $ AssignAn IType "i" $ Lit 0)
    <> update (URet $ Assign "ans" $ Lit True)
    <> while
      (CRet $ BinBool And (Var "ans") (BoolCalc IType S (Var "i") (Var "n")))
      ( cond
          (CRet $ BoolCalc CType NE (Elem (Var "pw") (Var "i")) (Elem (Var "gs") (Var "i")))
          ( update (URet $ Assign "ans" $ Lit False)
          )
          skip
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (Lit 1))
      )

hyperI :: String -> String -> Dist (Dist Literal)
hyperI pw gs = projectPw (hysemBobby (basicI (length pw)) (initialDist pw gs))

basicL :: Int -> Bobby
basicL n =
  update (URet $ Assign "n" $ Lit n)
    <> update (URet $ AssignAn IType "i" $ Lit 0)
    <> update (URet $ Assign "ans" $ Lit True)
    <> while
      (CRet $ BoolCalc IType S (Var "i") (Var "n"))
      ( cond
          (CRet $ BoolCalc CType NE (Elem (Var "pw") (Var "i")) (Elem (Var "gs") (Var "i")))
          ( update (URet $ Assign "ans" $ Lit False)
          )
          skip
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (Lit 1))
      )

hyperL :: String -> String -> Dist (Dist Literal)
hyperL pw gs = projectPw (hysemBobby (basicL (length pw)) (initialDist pw gs))

basicM :: Int -> Bobby
basicM n =
  update (URet $ Assign "n" $ Lit n)
    <> update (URet $ AssignAn IType "i" $ Lit 0)
    <> update (URet $ Assign "ans" $ Lit True)
    <> while
      (CRet $ BoolCalc IType S (Var "i") (Var "n"))
      ( update
          ( URet $
              Assign "ans" $
                BinBool
                  And
                  (Var "ans")
                  ( BoolCalc
                      CType
                      E
                      (Elem (Var "pw") (Var "i"))
                      (Elem (Var "gs") (Var "i"))
                  )
          )
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (Lit 1))
      )

hyperM :: String -> String -> Dist (Dist Literal)
hyperM pw gs = projectPw (hysemBobby (basicM (length pw)) (initialDist pw gs))

basicN :: Int -> Bobby
basicN n =
  update (URet $ Assign "n" $ Lit n)
    <> update (URet $ AssignAn IType "i" $ Lit 0)
    <> update (URet $ Assign "ans" $ Lit True)
    <> while
      (CRet $ BoolCalc IType S (Var "i") (Var "n"))
      ( update
          ( URet $
              Assign "ans" $
                BinBool
                  And
                  (Var "ans")
                  ( BoolCalc
                      CType
                      E
                      (Elem (Var "pw") (Var "i"))
                      (Elem (Var "gs") (Var "i"))
                  )
          )
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (Lit 1))
      )
    <> observe (ORet' BType (Var "ans"))

hyperN :: String -> String -> Dist (Dist Literal)
hyperN pw gs = projectPw (hysemBobby (basicN (length pw)) (initialDist pw gs))

basicR :: Int -> Bobby
basicR n =
  update (URet $ Assign "n" $ Lit n)
    <> update (URet $ Assign "l" $ Range (Lit 0) (IntCalc Sub (Var "n") (Lit 1)))
    <> update (URet $ AssignAn IType "i" $ Lit 0)
    <> update (URet $ Assign "ans" $ Lit True)
    <> while
      (CRet $ BinBool And (Var "ans") (BoolCalc (LType IType) NE (Var "l") (LitAn (LType IType) [])))
      ( update
          (UUniAssign IType "i" (Var "l"))
          <> update
            ( URet $
                Assign "ans" $
                  BinBool
                    And
                    (Var "ans")
                    ( BoolCalc
                        CType
                        E
                        (Elem (Var "pw") (Var "i"))
                        (Elem (Var "gs") (Var "i"))
                    )
            )
          <> update (URet $ AssignAn (LType IType) "l" $ ListDiv (Var "l") (Singleton (Var "i")))
      )
    <> observe (ORet' BType (Var "ans"))

hyperR :: String -> String -> Dist (Dist Literal)
hyperR pw gs = projectPw (hysemBobby (basicR (length pw)) (initialDist pw gs))

basicS :: Int -> Bobby
basicS n =
  update (URet $ Assign "n" $ Lit n)
    <> update (URet $ Assign "l" $ Range (Lit 0) (IntCalc Sub (Var "n") (Lit 1)))
    <> update (URet $ AssignAn IType "i" $ Lit 0)
    <> update (URet $ Assign "ans" $ Lit True)
    <> while
      (CRet $ BinBool And (Var "ans") (BoolCalc (LType IType) NE (Var "l") (LitAn (LType IType) [])))
      ( update
          (UUniAssign IType "i" (Var "l"))
          <> cond
            ( CRet $
                BoolCalc
                  CType
                  NE
                  (Elem (Var "pw") (Var "i"))
                  (Elem (Var "gs") (Var "i"))
            )
            (update (URet $ AssignAn BType "ans" $ Lit False))
            skip
          <> update (URet $ AssignAn (LType IType) "l" $ ListDiv (Var "l") (Singleton (Var "i")))
      )
    <> observe (ORet' BType (Var "ans"))

hyperS :: String -> String -> Dist (Dist Literal)
hyperS pw gs = projectPw (hysemBobby (basicS (length pw)) (initialDist pw gs))
