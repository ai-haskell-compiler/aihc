{-# LANGUAGE MagicHash #-}

module GHC.Integer.Logarithms
  ( integerLogBase#,
    integerLog2#,
    wordLog2#,
  )
where

import GHC.Num.Integer (Integer)
import GHC.Prim (Int#, Word#, word2Int#)
import GHC.Prim.Integer qualified as I

-- | The logarithm of a positive 'Integer' to a base greater than one,
-- rounded down.
integerLogBase# :: Integer -> Integer -> Int#
integerLogBase# base value = word2Int# (I.integerLogBase# base value)

-- | The base 2 logarithm of a positive 'Integer', rounded down.
integerLog2# :: Integer -> Int#
integerLog2# value = word2Int# (I.integerLog2# value)

-- | The base 2 logarithm of a 'Word#', rounded down.  @wordLog2# 0## = -1#@.
wordLog2# :: Word# -> Int#
wordLog2# value = word2Int# (I.wordLog2# value)
