module CharComparison where

data CharOp = Ec | NEc

charOpToFunc :: CharOp -> Char -> Char -> Bool
charOpToFunc Ec = (==)
charOpToFunc NEc = (/=)
