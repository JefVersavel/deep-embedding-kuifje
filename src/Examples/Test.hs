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
  file <- readFile "./Bobby/basicS.bobby"
  let e = parse bobbyParser "" file
  case e of
    Right ex -> do
      print $ basicS 3
      print ex
      print $ uhyperS "abc" "abc"
      print $
        projectPw $
          uHysemBobby
            ex
            (initialDist' "abc" "abc")
    Left m -> print m
