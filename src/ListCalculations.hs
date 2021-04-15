module ListCalculations where

import Data.List

data ListOp = LDiv | Union | Intersect | Concat
  deriving (Show)

listOpToFunc :: Eq a => ListOp -> [a] -> [a] -> [a]
listOpToFunc LDiv = (\\)
listOpToFunc Union = union
listOpToFunc Intersect = intersect
listOpToFunc Concat = (++)
