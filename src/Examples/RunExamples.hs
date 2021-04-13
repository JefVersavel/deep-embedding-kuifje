module Examples.RunExamples where

import Examples.Monty
import Examples.Password
import Examples.ReadMeExample
import Examples.ReadMeExampleKuifje
import Examples.SideChannel

mainTest = do
  print "ReadMeExample"
  print hyper
  print "ReadMeExampleKuifje"
  print hyperKuifje
  print "SideChannel"
  print "hyper2"
  print hyper2
  print "hyper235"
  print hyper235
  print "Monty"
  print monty
  print "Password"
  print "hyperI"
  print $ hyperI "abc" "abc"
  print "hyperL"
  print $ hyperL "abc" "abc"
  print "hyperM"
  print $ hyperM "abc" "abc"
  print "hyperN"
  print $ hyperN "abc" "abc"
  print "hyperR"
  print $ hyperR "abc" "abc"
  print "hyperS"
  print $ hyperS "abc" "abc"
