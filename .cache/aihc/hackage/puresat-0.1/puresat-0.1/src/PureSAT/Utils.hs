module PureSAT.Utils where

import Control.Monad.ST (ST)
import Data.Bits
import Data.STRef (STRef, readSTRef)

whenOk :: STRef s Bool -> ST s Bool -> ST s Bool
whenOk ok = whenOk_ (readSTRef ok)

whenOk_ :: ST s Bool -> ST s Bool -> ST s Bool
whenOk_ ok action = do
    ok' <- ok
    if ok' then action else return False

-- | Next power of two.
--
-- >>> map nextPowerOf2 [-1, 0, 1, 2, 1024, 1245, 9999]
-- [1,1,1,2,1024,2048,16384]
--
nextPowerOf2 :: Int -> Int
nextPowerOf2 n
    | n <= 1    = 1
    | otherwise = unsafeShiftL 1 (finiteBitSize (0 :: Int) - countLeadingZeros (n - 1))
