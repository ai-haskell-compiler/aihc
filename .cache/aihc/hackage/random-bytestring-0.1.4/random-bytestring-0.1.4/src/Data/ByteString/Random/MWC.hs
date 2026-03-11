{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- -------------------------------------------------------------------------- --
-- |
-- Module: Data.ByteString.Random.MWC
-- Copyright: (c) Lars Kuhtz <lakuhtz@gmail.com> 2017
-- License: MIT
-- Maintainer: lakuhtz@gmail.com
-- Stability: experimental

module Data.ByteString.Random.MWC
( random
, randomGen
) where

import Data.ByteString (ByteString)

import Numeric.Natural (Natural)

import System.Random.MWC (uniform, GenIO, withSystemRandom)

-- internal imports

import Data.ByteString.Random.Internal

-- -------------------------------------------------------------------------- --

instance (g ~ GenIO) ⇒ RandomWords g where
    uniformW8 = uniform
    {-# INLINE uniformW8 #-}
    uniformW64 = uniform
    {-# INLINE uniformW64 #-}

{-# SPECIALIZE generate ∷ GenIO → Natural → IO ByteString #-}

-- -------------------------------------------------------------------------- --

randomGen
    ∷ GenIO
        -- ^ PRNG
    → Natural
        -- ^ Length of the result bytestring in bytes
    → IO ByteString
randomGen = generate
{-# INLINE randomGen #-}

-- $setup
-- >>> import qualified Data.ByteString as B
-- >>> import Test.QuickCheck

-- | Generate a random bytestring of length n. The PRNG is seeded
-- from the system randomness source.
--
-- prop> ioProperty $ ((fromIntegral n ===) . B.length) <$> random n
-- prop> n > 4 ==> ioProperty $ (/=) <$> random n <*> random n
--
random
    ∷ Natural
        -- ^ Length of the result bytestring in bytes
    → IO ByteString
random = withSystemRandom . flip randomGen

