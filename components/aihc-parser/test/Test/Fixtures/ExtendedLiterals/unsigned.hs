{- ORACLE_TEST
id: unsigned
category: expressions
expected: pass
-}
{-# LANGUAGE ExtendedLiterals #-}
module Unsigned where

x = 123u8
y = 123u16
z = 123u32
w = 123u64
