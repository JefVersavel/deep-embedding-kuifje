module ListComparison where

data ListOp = El | NEl

listOpToFunc :: Eq a => ListOp -> ([a] -> [a] -> Bool)
listOpToFunc El = (==)
listOpToFunc NEl = (/=)
