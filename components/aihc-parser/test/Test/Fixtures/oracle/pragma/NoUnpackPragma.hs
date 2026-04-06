{- ORACLE_TEST xfail NOUNPACK pragma not preserved in pretty-printer roundtrip -}
module NoUnpackPragma where

data NoUnpack = NoUnpack {-# NOUNPACK #-} !(Int, Int)
