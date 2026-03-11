module Data.Flag.Internal where


import Data.Word


type Flag = Word64
-- | Hard coded just for `Word64`. Should be improved
bitLen = 64

-- | Check `maxBound` of `Enum e` whether it have more than `bitLen` elements
isFlaggable :: (Bounded e, Enum e) => e -> Bool
isFlaggable x = fromEnum (maxBound `asTypeOf` x) < bitLen
