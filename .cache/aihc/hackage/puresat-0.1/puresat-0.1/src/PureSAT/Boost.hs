module PureSAT.Boost where

import Data.Bits (unsafeShiftR)

-- | Boost weight
--
-- >>> boost 0
-- 64
--
-- >>> boost 3
-- 67
--
-- >>> boost maxBound
-- 18446744073709551615
--
-- >>> boost (maxBound - 63)
-- 18446744073709551615
--
-- >>> boost (maxBound - 64)
-- 18446744073709551615
--
boost :: Word -> Word
boost !n =
    let !m = n + 64
    in if m < n then maxBound else m
{-# INLINE [1] boost #-}

-- | Decay weight
--
-- >>> decay 0
-- 0
--
-- >>> decay 1
-- 0
--
-- >>> decay 40
-- 39
--
-- >>> decay 10000
-- 9844
--
decay :: Word -> Word
decay 0 = 0
decay n = n - max 1 (unsafeShiftR n 6)
{-# INLINE [1] decay #-}
