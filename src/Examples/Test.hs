module Examples.Test where

import Data.Map
import Examples.UReadMeExample
import Language.Kuifje.Distribution
import Language.Kuifje.Syntax
import Parsing
import Semantics
import State
import Syntax
import Text.ParserCombinators.Parsec
import Prelude hiding (return)

p = do
  file <- readFile "./Bobby/readMeExample.bobby"
  let e = parse bobbyParser "" file
  print e
  case e of
    Right ex -> do
      print $ program
      print $ ex
      print $ project $ uHysemBobby program (uniform [initStore x | x <- [5 .. 8]])
      print $ project $ uHysemBobby ex $return $ Store empty
    Left m -> print m
