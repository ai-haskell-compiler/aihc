{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Data.ByteString.Random.Internal
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017
-- License: MIT
-- Maintainer: lakuhtz@gmail.com
-- Stability: experimental

module Data.ByteString.Random.Internal
( generate
, RandomWords(..)
) where

import Control.Exception (bracketOnError)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.Word (Word8, Word64)

import Foreign (mallocBytes, poke, plusPtr, free, castPtr)

import GHC.Ptr (Ptr(..))

import Numeric.Natural (Natural)

-- -------------------------------------------------------------------------- --

class RandomWords g where
    uniformW8 ∷ g → IO Word8
        -- ^ function that generates uniformily distributed random 8 bit words
    uniformW64 ∷ g → IO Word64
        -- ^ function that generates uniformily distributed random 64 bit words

-- The reason why a type class is used instead of passing the IO functions
-- directoy to generate, is that we can force GHC to inline these methods by
-- using a SPECIALIZE pragma.

-- -------------------------------------------------------------------------- --

-- | Generates uniformily distributed random bytestrings of length n using the
-- given PRNG.
--
generate
    ∷ RandomWords g
    ⇒ g
        -- ^ PRNG
    → Natural
        -- ^ Length of the result bytestring in bytes
    → IO ByteString
generate g n =
    bracketOnError (mallocBytes len8) free $ \ptr@(Ptr !addr) → do
        {-# SCC "go" #-} go ptr
        {-# SCC "pack" #-} unsafePackAddressLen len8 addr
  where
    len8, len64 ∷ Int
    !len8 = fromIntegral n
    !len64 = len8 `div` 8

    go ∷ Ptr Word64 → IO ()
    go !startPtr = loop64 startPtr
      where
        -- Would it help to add more manual unrolling levels?
        -- How smart is the compiler about unrolling loops?

        -- Generate 64bit values
        fin64Ptr ∷ Ptr Word64
        !fin64Ptr = startPtr `plusPtr` (len64 * 8)

        loop64 ∷ Ptr Word64 → IO ()
        loop64 !curPtr
            | curPtr < fin64Ptr = {-# SCC "loop64" #-} do
                !b ← uniformW64 g
                {-# SCC "poke64" #-} poke curPtr b
                loop64 $ curPtr `plusPtr` 8
            | otherwise = loop8 $ castPtr curPtr

        -- Generate 8bit values
        fin8Ptr ∷ Ptr Word8
        !fin8Ptr = startPtr `plusPtr` len8

        loop8 ∷ Ptr Word8 → IO ()
        loop8 !curPtr
            | curPtr < fin8Ptr = {-# SCC "loop8" #-} do
                !b ← uniformW8 g
                poke curPtr b
                loop8 $ curPtr `plusPtr` 1
            | otherwise = return ()
{-# INLINEABLE generate #-}

