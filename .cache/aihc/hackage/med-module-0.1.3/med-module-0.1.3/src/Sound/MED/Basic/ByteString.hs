module Sound.MED.Basic.ByteString where

import qualified Data.ByteString as B

import Data.Bits (Bits, shiftL, (.|.))
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int8, Int16, Int32)


type PTR = Word32
type Peek a = B.ByteString -> PTR -> a


peekInt32 :: Peek Int32; peekInt32 xs = fromIntegral . peekWord32 xs
peekInt16 :: Peek Int16; peekInt16 xs = fromIntegral . peekWord16 xs
peekInt8  :: Peek Int8;  peekInt8  xs = fromIntegral . peekWord8  xs

peekWord32 :: Peek Word32
peekWord32 xs ptr =
  let k = fromIntegral ptr
  in  peekOffset xs k 0 <+ peekOffset xs k 1 <+
      peekOffset xs k 2 <+ peekOffset xs k 3

peekWord16 :: Peek Word16
peekWord16 xs ptr =
  let k = fromIntegral ptr
  in  peekOffset xs k 0 <+ peekOffset xs k 1

infixl 6 <+
(<+) :: (Bits a) => a -> a -> a
x <+ y = shiftL x 8 .|. y

peekOffset :: (Bits a, Num a) => B.ByteString -> Int -> Int -> a
peekOffset xs k d = fromIntegral $ B.index xs $ k+d

peekWord8 :: Peek Word8
peekWord8 xs k = B.index xs $ fromIntegral k
