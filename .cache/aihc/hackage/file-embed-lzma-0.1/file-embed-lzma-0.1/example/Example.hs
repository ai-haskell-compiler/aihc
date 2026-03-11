{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import Data.Foldable (for_)
import FileEmbedLzma (embedByteString, embedRecursiveDir)

import qualified Data.ByteString as BS

main :: IO ()
main = do
    BS.putStr $$(embedByteString "example/example.txt")
    for_ $$(embedRecursiveDir "example") $ \(n, bs) ->
        print (n, BS.length bs)
