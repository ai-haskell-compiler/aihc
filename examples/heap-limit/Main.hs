module Main (main) where

-- The program is run with a heap limit of one byte, which it exceeds before
-- it can print anything. The runtime reports the limit and exits with a
-- failure status.
main :: IO ()
main = print (sum [1 .. 100000 :: Int])
