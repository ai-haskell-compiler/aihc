{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
module X where

foreign import capi unsafe "HsXXHash.h hs_XXH3_64bits_withSeed_offset" unsafe_xxh3_64bit_withSeed_ba :: Int -> Int
