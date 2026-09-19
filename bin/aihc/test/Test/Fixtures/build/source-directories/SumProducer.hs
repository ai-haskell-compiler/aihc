{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module SumProducer (produce, choose) where

import GHC.Exts (Int#, (+#))

produce :: Int# -> (# (# #) | (# Int#, Int# #) #)
produce 0# = (# (# #) | #)
produce n = (# | (# n, n +# 1# #) #)

choose :: Bool -> a -> (# (# #) | a #)
choose False _ = (# (# #) | #)
choose True value = (# | value #)
