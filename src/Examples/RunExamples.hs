module Examples.RunExamples where

import Examples.Monty
import Examples.Password
import Examples.ReadMeExample
import Examples.ReadMeExampleKuifje
import Examples.SideChannel
import Examples.UMonty
import Examples.UPassword
import Examples.UReadMeExample
import Examples.USideChannel

mainTest = do
  print "ReadMeExample"
  print hyper
  print "UReadMeExample"
  print uhyper
  print "ReadMeExampleKuifje"
  print hyperKuifje
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
  print "Monty"
  print monty
  print "UMonty"
  print umonty
  print "Password/UPassword"
  print "hyperI"
  print $ hyperI "abc" "abc"
  print "uhyperI"
  print $ uhyperI "abc" "abc"
  print "hyperL"
  print $ hyperL "abc" "abc"
  print "uhyperL"
  print $ uhyperL "abc" "abc"
  print "hyperM"
  print $ hyperM "abc" "abc"
  print "uhyperM"
  print $ uhyperM "abc" "abc"
  print "hyperN"
  print $ hyperN "abc" "abc"
  print "uhyperN"
  print $ uhyperN "abc" "abc"
  print "hyperR"
  print $ hyperR "abc" "abc"
  print "uhyperR"
  print $ uhyperR "abc" "abc"
  print "hyperS"
  print $ hyperS "abc" "abc"
  print "uhyperS"
  print $ uhyperS "abc" "abc"
