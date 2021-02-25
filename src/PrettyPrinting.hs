module PrettyPrinting where

import Data.Map
import Language.Kuifje.Distribution

type S a = Map String a

showS :: (Show a, Show b) => Map a b -> String
showS s = listOfTuplesToString list
  where
    list = toList s

listOfTuplesToString :: (Show a, Show b) => [(a, b)] -> String
listOfTuplesToString [] = ""
listOfTuplesToString ((k, v) : rest) =
  "[ " ++ show k ++ "\t -> \t " ++ show v ++ "] \n"
    ++ listOfTuplesToString rest

showDist :: Show a => Dist (S a) -> String
showDist d = distToString distList
  where
    distList = toList $ runD d

distEntryToString :: Show a => S a -> Prob -> String
distEntryToString s p = "Probability " ++ show p ++ ": \n" ++ showS s ++ "\n"

distToString :: Show a => [(S a, Prob)] -> String
distToString [] = ""
distToString ((s, p) : rest) = distEntryToString s p ++ distToString rest

printDist :: Show a => Dist (S a) -> IO ()
printDist d = putStr $ showDist d
