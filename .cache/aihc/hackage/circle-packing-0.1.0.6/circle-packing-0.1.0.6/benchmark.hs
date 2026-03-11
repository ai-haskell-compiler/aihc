import Criterion.Main
import Optimisation.CirclePacking

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

maxSize = 9

sets = 
    [ ("Widly varying", take (2^n) $ [1,2..]) | n <- [4..maxSize] ] ++
    [ ("Slightly varying", take (2^n) $ cycle [1,1.1..10]) | n <- [4..maxSize] ] ++
    [ ("Equal size", take (2^n) $ repeat 1) | n <- [4..maxSize] ]

proc cs = packCircles id cs

main = defaultMain $
    [ bench (desc ++ " (" ++ show (length c) ++ " circles)") $ nf proc c |
        (desc, c) <- sets ]
