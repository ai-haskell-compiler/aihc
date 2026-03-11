module Main where

import Prelude hiding ((&&), (||), not, all)
import Data.Algebra.Boolean ((&&), (||), not, all)
import Data.Mapping
import Data.Mapping.Decision


main :: IO ()
main = do
  do
    let x = test "x" :: Decision Bool OnBool String Bool
    let y = test "y"

    putStrLn "  x"
    putStrLn "-----"
    putStrLn $ debugShow x
    putStrLn ""

    putStrLn "  not x"
    putStrLn "---------"
    putStrLn $ debugShow (not x)
    putStrLn ""

    putStrLn "  x && y"
    putStrLn "----------"
    putStrLn $ debugShow (x && y)
    putStrLn ""

    putStrLn "  y && x"
    putStrLn "----------"
    putStrLn $ debugShow (y && x)
    putStrLn ""

    putStrLn "  x || y"
    putStrLn "----------"
    putStrLn $ debugShow (x || y)
    putStrLn ""

    putStrLn "  y || x"
    putStrLn "----------"
    putStrLn $ debugShow (y || x)
    putStrLn ""

  do
    putStrLn "  independent sets in C_100"
    putStrLn "-----------------------------"
    let l2 = (100,1):[(n,n+1) | n <- [1..99]]
    let l3 = (99,100,1):(100,1,2):[(n,n+1,n+2) | n <- [1..98]]
    let independent = all (\(i,j) -> not (test i && test j)) l2
    let maximal = all (\(i,j,k) -> test i || test j || test k) l3
    let t = independent && maximal :: Decision Bool OnBool Int Bool
    putStrLn $ debugShow t
