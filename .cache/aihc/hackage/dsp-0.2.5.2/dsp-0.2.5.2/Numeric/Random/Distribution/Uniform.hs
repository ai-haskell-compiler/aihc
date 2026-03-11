-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Random.Distribution.Uniform
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for turning a list of random integers (as 'Word32') in a list
-- of Uniform RV's
--
-----------------------------------------------------------------------------

module Numeric.Random.Distribution.Uniform where

import Data.Word

-- Float  : 1 sign, 8 exp,  23 fraction
-- Double : 1 sign, 11 exp, 52 fraction

-- | 32 bits in [0,1]

-- 4294967295 = 2^32 - 1

uniform32cc :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform32cc xs = map ((/ 4294967295.0) . fromIntegral) $ xs

-- | 32 bits in [0,1)

-- 4294967296 = 2^32

uniform32co :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform32co xs = map ((/ 4294967296.0) . fromIntegral) $ xs

-- | 32 bits in (0,1]

uniform32oc :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform32oc xs = filter (/= 0) $ uniform32cc $ xs

-- | 32 bits in (0,1)

uniform32oo :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform32oo xs = filter (/= 1) $ uniform32oc $ xs

-- | 53 bits in [0,1], ie 64-bit IEEE 754 in [0,1]

-- 67108864 = 2^26
-- 9007199254740991 = 2^53 - 1

uniform53cc :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform53cc xs = uniform' xs
    where uniform' (u1:u2:us) = (a * 67108864.0 + b) / 9007199254740991.0 : uniform' us
              where a = fromIntegral u1 / 32.0 -- 27 bits
                    b = fromIntegral u2 / 64.0 -- 26 bits
          uniform' _ = error "uniform53cc: input list must be infinite"

-- | 53 bits in [0,1), ie 64-bit IEEE 754 in [0,1)

-- 67108864 = 2^26
-- 9007199254740992 = 2^53

uniform53co :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform53co xs = uniform' $ xs
    where uniform' (u1:u2:us) = (a * 67108864.0 + b) / 9007199254740992.0 : uniform' us
              where a = fromIntegral u1 / 32.0 -- 27 bits
                    b = fromIntegral u2 / 64.0 -- 26 bits
          uniform' _ = error "uniform53co: input list must be infinite"

-- | 53 bits in (0,1]

uniform53oc :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform53oc xs = filter (/= 0) $ uniform53cc $ xs

-- | 53 bits in (0,1)

uniform53oo :: [Word32] -- ^ X
            -> [Double] -- ^ U

uniform53oo xs = filter (/= 1) $ uniform53oc $ xs

-- | transforms uniform [0,1] to [a,b]

uniform :: Double   -- ^ a
        -> Double   -- ^ b
        -> [Double] -- ^ U
        -> [Double] -- ^ U'

uniform a b us = map (\u -> (b-a)*u + a) us
