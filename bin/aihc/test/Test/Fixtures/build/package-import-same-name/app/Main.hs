module Main (main) where

import "this" Shared.Types (Wrapped (..), unwrap)

main :: IO ()
main = print (unwrap (Wrapped 41))
