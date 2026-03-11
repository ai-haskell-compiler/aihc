module Main where

import qualified Test.DocTest as Doc

main :: IO ()
main = Doc.doctest
    [ "-isrc"
    , "src/Control/Concurrent/WVar.hs"
    , "src/Control/Concurrent/KazuraQueue.hs"
    ]
