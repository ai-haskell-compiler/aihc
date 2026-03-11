{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Random collection of utility functions that may be useful, but which aren't
   useful enough to be included in the main API modules.
-}

module OpenSSL.Util where

import Data.Word
import Numeric

-- |Neat helper to pretty-print digests into the common hexadecimal notation:
--
-- >>> [0..15] >>= toHex
-- "000102030405060708090a0b0c0d0e0f"

toHex :: Word8 -> String
toHex w = case showHex w "" of
           [w1,w2] -> [w1, w2]
           [w2]    -> ['0', w2]
           _       -> "showHex returned []"
