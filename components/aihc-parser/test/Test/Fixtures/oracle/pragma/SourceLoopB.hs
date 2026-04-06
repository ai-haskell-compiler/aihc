{- ORACLE_TEST xfail SOURCE pragma in import not preserved in pretty-printer roundtrip -}
module SourceLoopB where

import {-# SOURCE #-} SourceLoopA

data B = B A | B0

fromB :: B -> Int
fromB (B a) = fromA a
fromB B0 = 0
