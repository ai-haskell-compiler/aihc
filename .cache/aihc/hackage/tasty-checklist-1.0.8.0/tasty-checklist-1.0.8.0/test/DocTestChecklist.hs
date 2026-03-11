module Main ( main ) where

import qualified Test.DocTest as T

main :: IO ()
main = T.doctest ["-isrc", "src/Test/Tasty/Checklist.hs"]
