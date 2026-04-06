{- ORACLE_TEST xfail SOURCE pragma in import not preserved in pretty-printer roundtrip -}
module SourceLoopA where

import {-# SOURCE #-} SourceLoopB

data A = A B

fromA :: A -> Int
fromA (A b) = fromB b
