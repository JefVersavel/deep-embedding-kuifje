module Examples.Test where

import Data.Map
import Examples.UMonty
import Examples.UPassword
import Examples.UReadMeExample
import Examples.USideChannel
import Language.Kuifje.Distribution
import Language.Kuifje.Syntax
import Parsing
import Semantics
import State
import Syntax
import Text.ParserCombinators.Parsec
import Prelude hiding (return)

em = return $ Store empty

p = do
  file <- readFile "./Bobby/sideChannel2.bobby"
  let e = parse bobbyParser "" file
  case e of
    Right ex -> do
      print ex
      print $ sideProject $ uHysemBobby ex em
    Left m -> print m
