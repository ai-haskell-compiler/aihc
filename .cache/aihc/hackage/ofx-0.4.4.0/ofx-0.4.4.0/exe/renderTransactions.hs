-- | Reads OFX file on standard input, and gets all transactions.
-- Pretty prints the result to standard output.

module Main where

import Data.OFX

main :: IO ()
main = interact prettyRenderTransactions
