module Sound.MED.Basic.Storable where

import Foreign.Storable (Storable, peekByteOff)
import Foreign.Ptr (Ptr)

import Control.Applicative ((<$>))

import Data.Storable.Endian (HasBigEndian, getBigEndian)
import Data.Word (Word32)


type MEM = Ptr ()
type PTR = Word32
type Peek a = MEM -> PTR -> IO a

peekOffset :: (Storable a) => Peek a
peekOffset mem ptr = peekByteOff mem (fromIntegral ptr)

peekBig :: (Storable a, HasBigEndian a) => Peek a
peekBig mem ptr = getBigEndian <$> peekOffset mem ptr
