{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module PatternGuardTypeSignatureUnboxedSum where

(#
      |
      | []
      |  #) =
   if | (# #) <- (() :: (# (# [a||] | C #) | * #))
        -> 0
