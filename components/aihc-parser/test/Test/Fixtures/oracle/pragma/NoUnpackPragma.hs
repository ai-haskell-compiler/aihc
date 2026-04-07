{- ORACLE_TEST pass -}
module NoUnpackPragma where

data NoUnpack = NoUnpack {-# NOUNPACK #-} !(Int, Int)
