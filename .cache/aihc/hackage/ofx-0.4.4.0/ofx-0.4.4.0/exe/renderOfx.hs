-- | Reads OFX file on standard input.  Parses it and pretty prints
-- the result to standard output.

module Main where

import Data.OFX

main :: IO ()
main = interact prettyRenderOfxFile
