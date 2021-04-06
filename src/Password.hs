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
          -- also add string comparison
          (CRet $ CharBool NEc (Elem (Var "pw") (Var "i")) (Elem (Var "gs") (Var "i")))
          ( update (URet $ Assign "ans" $ BoolLit False)
          )
          skip
          <> update (URet $ Assign "i" $ IntCalc Add (Var "i") (IntLit 1))
      )

hyperI :: String -> String -> Dist (Dist Literal)
hyperI pw gs = projectPw (hysemBobby (basicI (length pw)) (initialDist pw gs))
