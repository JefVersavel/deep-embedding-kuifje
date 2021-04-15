module Arithmetic where

data BinOpAri
  = Add
  | Sub
  | Mul
  | Div
  | Power
  | Mod
  deriving (Show)

opAriToFunc :: BinOpAri -> Int -> Int -> Int
opAriToFunc Add = (+)
opAriToFunc Sub = (-)
opAriToFunc Mul = (*)
opAriToFunc Div = div
opAriToFunc Power = (^)
opAriToFunc Mod = mod
