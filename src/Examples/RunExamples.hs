module Examples.RunExamples where

import Data.Either
import Data.Map
import Examples.Monty
import Examples.Password
import Examples.ReadMeExample
import Examples.ReadMeExampleKuifje
import Examples.SideChannel
import Examples.UMonty
import Examples.UPassword
import Examples.UReadMeExample
import Examples.USideChannel
import qualified Language.Kuifje.Distribution as D
import Parsing
import Semantics
import State
import Text.ParserCombinators.Parsec
import Prelude

em = D.return $ Store empty

unsafeParse path = do
  file <- readFile path
  case parse bobbyParser "" file of
    Right e -> return e
    Left _ -> error "cannot parse"

mainTest = do
  print "ReadMeExample"
  print hyper
  print "UReadMeExample"
  print uhyper
  print "readExampleBobby"
  readme <- unsafeParse "../Examples/Bobby/readMeExample.bobby"
  print $ Examples.UReadMeExample.project $ uHysemBobby readme em
  print "SideChannel"
  print "hyper2"
  print hyper2
  print "hyper235"
  print hyper235
  print "USideChannel"
  print "uhyper2"
  print uhyper2
  print "uhyper235"
  print uhyper235
  print "sideChannelBobby"
  print "hyper2Bobby"
  h2 <- unsafeParse "../Examples/Bobby/sideChannel2.bobby"
  print $ Examples.USideChannel.sideProject $ uHysemBobby h2 em
  print "hyper235Bobby"
  h235 <- unsafeParse "../Examples/Bobby/sideChannel235.bobby"
  print $ Examples.USideChannel.sideProject $ uHysemBobby h235 em
  print "Monty"
  print monty
  print "UMonty"
  print umonty
  print "MontyBobby"
  montyBobby <- unsafeParse "../Examples/Bobby/monty.bobby"
  print $ uHysemBobby montyBobby em
  print "Password/UPassword"
  print "hyperI"
  print $ hyperI "abc" "abc"
  print "uhyperI"
  print $ uhyperI "abc" "abc"
  print "hyperIBobby"
  hyperIBobby <- unsafeParse "../Examples/Bobby/basicI.bobby"
  print $ Examples.UPassword.projectPw $ uHysemBobby hyperIBobby (initialDist' "abc" "abc")
  print "hyperL"
  print $ hyperL "abc" "abc"
  print "uhyperL"
  print $ uhyperL "abc" "abc"
  print "hyperLBobby"
  hyperLBobby <- unsafeParse "../Examples/Bobby/basicL.bobby"
  print $ Examples.UPassword.projectPw $ uHysemBobby hyperLBobby (initialDist' "abc" "abc")
  print "hyperM"
  print $ hyperM "abc" "abc"
  print "uhyperM"
  print $ uhyperM "abc" "abc"
  print "hyperMBobby"
  hyperMBobby <- unsafeParse "../Examples/Bobby/basicM.bobby"
  print $ Examples.UPassword.projectPw $ uHysemBobby hyperMBobby (initialDist' "abc" "abc")
  print "hyperN"
  print $ hyperN "abc" "abc"
  print "uhyperN"
  print $ uhyperN "abc" "abc"
  print "hyperNBobby"
  hyperNBobby <- unsafeParse "../Examples/Bobby/basicN.bobby"
  print $ Examples.UPassword.projectPw $ uHysemBobby hyperNBobby (initialDist' "abc" "abc")
  print "hyperR"
  print $ hyperR "abc" "abc"
  print "uhyperR"
  print $ uhyperR "abc" "abc"
  print "hyperRBobby"
  hyperRBobby <- unsafeParse "../Examples/Bobby/basicR.bobby"
  print $ Examples.UPassword.projectPw $ uHysemBobby hyperRBobby (initialDist' "abc" "abc")
  print "hyperS"
  print $ hyperS "abc" "abc"
  print "uhyperS"
  print $ uhyperS "abc" "abc"
  print "hyperSBobby"
  hyperSBobby <- unsafeParse "../Examples/Bobby/basicS.bobby"
  print $ Examples.UPassword.projectPw $ uHysemBobby hyperSBobby (initialDist' "abc" "abc")
