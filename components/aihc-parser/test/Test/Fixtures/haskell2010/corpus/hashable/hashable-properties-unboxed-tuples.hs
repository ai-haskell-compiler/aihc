{- ORACLE_TEST
id: hashable-properties-unboxed-tuples
category: corpus
expected: pass
reason: from hashable/tests/Properties.hs; parser supports unboxed tuple syntax
-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module X where

import GHC.Exts (Int#)

f :: Int# -> (# Int#, Int# #)
f x = (# x, x #)
