module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (bm:test)
import qualified BM.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test"
    [ BM.Test.tests
    ]
