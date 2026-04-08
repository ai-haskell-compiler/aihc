{- ORACLE_TEST pass -}
module SourceLoopA where

import {-# SOURCE #-} SourceLoopB

data A = A B

fromA :: A -> Int
fromA (A b) = fromB b
