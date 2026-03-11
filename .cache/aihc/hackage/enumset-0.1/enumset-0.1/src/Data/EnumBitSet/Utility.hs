module Data.EnumBitSet.Utility where

import qualified Data.Bits as B
import Data.Bits (Bits, bit, xor, (.&.), )


-- fixity like .&.
infixl 7 .-.

(.-.) :: (Bits w) => w -> w -> w
x .-. y = x .&. B.complement y

empty :: (Bits w) => w
empty = xor (bit 0) (bit 0)
