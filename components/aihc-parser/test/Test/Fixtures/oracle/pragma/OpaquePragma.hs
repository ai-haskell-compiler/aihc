{- ORACLE_TEST xfail OPAQUE pragma not preserved in pretty-printer roundtrip -}
module OpaquePragma where

opaqueFun :: Int -> Int
opaqueFun x = x + 1
{-# OPAQUE opaqueFun #-}
