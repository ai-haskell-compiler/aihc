module Data.Hash.Instances ( Hashable(..), hashFoldable )

where

import Data.Hash.Base

import Data.Word
import Data.Int
import Data.Ratio
import Data.Foldable

class Hashable a where
    hash :: a -> Hash

hashFoldable :: (Foldable t, Hashable a) => t a -> Hash
hashFoldable = foldl' (\h a -> h `combine` hash a) h0


instance Hashable Word8 where
    hash = hashWord8

instance Hashable Word16 where
    hash = hashWord16

instance Hashable Word32 where
    hash = hashWord32

instance Hashable Word64 where
    hash = hashWord64

instance Hashable Word where
    hash w = hash (fromIntegral w :: Int)

instance Hashable Int8 where
    hash i = hash (fromIntegral i :: Word8)

instance Hashable Int16 where
    hash i = hash (fromIntegral i :: Word16)

instance Hashable Int32 where
    hash i = hash (fromIntegral i :: Word32)

instance Hashable Int64 where
    hash i = hash (fromIntegral i :: Word64)

instance Hashable Int where
    hash = hashInt

instance Hashable Integer where
    -- a very inefficient instance, but i don't have time to mess
    -- with internal representations now...
    hash = hash . show

instance Hashable Float where
    hash = hashStorable

instance Hashable Double where
    hash = hashStorable

instance (Integral a, Hashable a) => Hashable (Ratio a) where
    hash r = (hash $ numerator r) `combine` (hash $ denominator r)

instance Hashable Char where
    hash = hash . fromEnum

instance Hashable a => Hashable [a] where
    hash = hashFoldable

instance Hashable Bool where
    hash = hash . fromEnum

instance Hashable () where
    hash () = h0

instance (Hashable a, Hashable b) => Hashable (a,b) where
    hash (a,b) = hash a `combine` hash b

instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c) where
    hash (a,b,c) = hash a `combine` hash b `combine` hash c

instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (a,b,c,d)
  where
    hash (a,b,c,d) = hash a `combine` hash b `combine` hash c `combine` hash d

instance Hashable a => Hashable (Maybe a) where
    hash = maybe hF (\a -> hash a `combine` hT)

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    hash (Left  a) = hash a `combine` hT
    hash (Right b) = hash b `combine` hF
