module Example where

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : append xs ys
