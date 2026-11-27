module Clock (afterEpoch) where

import Foreign.C.Types (CDouble (..))

foreign import ccall unsafe "CFAbsoluteTimeGetCurrent" c_absoluteTime :: IO CDouble

-- | CoreFoundation counts its time from 2001. Each run is after that time.
afterEpoch :: IO Bool
afterEpoch = (> 0) <$> c_absoluteTime
