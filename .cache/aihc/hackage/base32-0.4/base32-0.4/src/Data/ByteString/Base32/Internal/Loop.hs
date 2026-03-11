{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base32.Internal.Loop
( innerLoop
, decodeLoop
) where

import Data.Bits
import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base32.Internal.Utils
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.Exts
import GHC.Word


-- ------------------------------------------------------------------------ --
-- Encoding loops

innerLoop
    :: Addr#
    -> Ptr Word64
    -> Ptr Word8
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ByteString)
    -> IO ByteString
innerLoop !lut !dptr !sptr !end finish = go dptr sptr
  where
    lix a = w64 (aix (fromIntegral a .&. 0x1f) lut)
    {-# INLINE lix #-}

    go !dst !src
      | plusPtr src 4 >= end = finish (castPtr dst) src
      | otherwise = do
        !t <- peekWord32BE (castPtr src)
        !u <- w32 <$> peek (plusPtr src 4)

        let !a = lix (unsafeShiftR t 27)
            !b = lix (unsafeShiftR t 22)
            !c = lix (unsafeShiftR t 17)
            !d = lix (unsafeShiftR t 12)
            !e = lix (unsafeShiftR t 7)
            !f = lix (unsafeShiftR t 2)
            !g = lix (unsafeShiftL t 3 .|. unsafeShiftR u 5)
            !h = lix u

        let !w = a
             .|. unsafeShiftL b 8
             .|. unsafeShiftL c 16
             .|. unsafeShiftL d 24
             .|. unsafeShiftL e 32
             .|. unsafeShiftL f 40
             .|. unsafeShiftL g 48
             .|. unsafeShiftL h 56

        poke dst w
        go (plusPtr dst 8) (plusPtr src 5)
{-# INLINE innerLoop #-}

-- ------------------------------------------------------------------------ --
-- Decoding loops

decodeLoop
    :: Addr#
    -> ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO (Either Text ByteString)
decodeLoop !lut !dfp !dptr !sptr !end = go dptr sptr
  where
    lix a = w64 (aix (fromIntegral a) lut)

    err :: Ptr Word8 -> IO (Either Text ByteString)
    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr :: Ptr Word8 -> IO (Either Text ByteString)
    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    look :: Ptr Word8 -> IO Word64
    look !p = lix <$> peek @Word8 p

    go !dst !src
      | plusPtr src 8 >= end = do

        a <- look src
        b <- look (plusPtr src 1)
        c <- look (plusPtr src 2)
        d <- look (plusPtr src 3)
        e <- look (plusPtr src 4)
        f <- look (plusPtr src 5)
        g <- look (plusPtr src 6)
        h <- look (plusPtr src 7)
        finalChunk dst src a b c d e f g h

      | otherwise = do
        !t <- peekWord64BE (castPtr src)

        let a = lix (unsafeShiftR t 56)
            b = lix (unsafeShiftR t 48)
            c = lix (unsafeShiftR t 40)
            d = lix (unsafeShiftR t 32)
            e = lix (unsafeShiftR t 24)
            f = lix (unsafeShiftR t 16)
            g = lix (unsafeShiftR t 8)
            h = lix t

        decodeChunk dst src a b c d e f g h

    finalChunk !dst !src !a !b !c !d !e !f !g !h
      | a == 0x63 = padErr src
      | b == 0x63 = padErr (plusPtr src 1)
      | a == 0xff = err src
      | b == 0xff = err (plusPtr src 1)
      | c == 0xff = err (plusPtr src 2)
      | d == 0xff = err (plusPtr src 3)
      | e == 0xff = err (plusPtr src 4)
      | f == 0xff = err (plusPtr src 5)
      | g == 0xff = err (plusPtr src 6)
      | h == 0xff = err (plusPtr src 7)
      | otherwise = do

        let !o1 = (fromIntegral a `unsafeShiftL` 3) .|. (fromIntegral b `unsafeShiftR` 2)
            !o2 = (fromIntegral b `unsafeShiftL` 6)
              .|. (fromIntegral c `unsafeShiftL` 1)
              .|. (fromIntegral d `unsafeShiftR` 4)
            !o3 = (fromIntegral d `unsafeShiftL` 4) .|. (fromIntegral e `unsafeShiftR` 1)
            !o4 = (fromIntegral e `unsafeShiftL` 7)
              .|. (fromIntegral f `unsafeShiftL` 2)
              .|. (fromIntegral g `unsafeShiftR` 3)
            !o5 = (fromIntegral g `unsafeShiftL` 5) .|. fromIntegral h

        poke @Word8 dst o1
        poke @Word8 (plusPtr dst 1) o2

        case (c,d,e,f,g,h) of
          (0x63,0x63,0x63,0x63,0x63,0x63) ->
            return (Right (BS dfp (1 + minusPtr dst dptr)))
          (0x63,_,_,_,_,_) -> padErr (plusPtr src 3)
          (_,0x63,0x63,0x63,0x63,0x63) -> padErr (plusPtr src 3)
          (_,0x63,_,_,_,_) -> padErr (plusPtr src 4)
          (_,_,0x63,0x63,0x63,0x63) -> do
            poke @Word8 (plusPtr dst 2) o3
            return (Right (BS dfp (2 + minusPtr dst dptr)))
          (_,_,0x63,_,_,_) -> padErr (plusPtr src 5)
          (_,_,_,0x63,0x63,0x63) -> do
            poke @Word8 (plusPtr dst 2) o3
            poke @Word8 (plusPtr dst 3) o4
            return (Right (BS dfp (3 + minusPtr dst dptr)))
          (_,_,_,0x63,_,_) -> padErr (plusPtr src 6)
          (_,_,_,_,0x63,0x63) -> padErr (plusPtr src 6)
          (_,_,_,_,0x63,_) -> padErr (plusPtr src 7)
          (_,_,_,_,_,0x63) -> do
            poke @Word8 (plusPtr dst 2) o3
            poke @Word8 (plusPtr dst 3) o4
            poke @Word8 (plusPtr dst 4) o5
            return (Right (BS dfp (4 + minusPtr dst dptr)))
          (_,_,_,_,_,_) -> do
            poke @Word8 (plusPtr dst 2) o3
            poke @Word8 (plusPtr dst 3) o4
            poke @Word8 (plusPtr dst 4) o5
            return (Right (BS dfp (5 + minusPtr dst dptr)))

    decodeChunk !dst !src !a !b !c !d !e !f !g !h
      | a == 0x63 = padErr src
      | b == 0x63 = padErr (plusPtr src 1)
      | c == 0x63 = padErr (plusPtr src 2)
      | d == 0x63 = padErr (plusPtr src 3)
      | e == 0x63 = padErr (plusPtr src 4)
      | f == 0x63 = padErr (plusPtr src 5)
      | g == 0x63 = padErr (plusPtr src 6)
      | h == 0x63 = padErr (plusPtr src 7)
      | a == 0xff = err src
      | b == 0xff = err (plusPtr src 1)
      | c == 0xff = err (plusPtr src 2)
      | d == 0xff = err (plusPtr src 3)
      | e == 0xff = err (plusPtr src 4)
      | f == 0xff = err (plusPtr src 5)
      | g == 0xff = err (plusPtr src 6)
      | h == 0xff = err (plusPtr src 7)
      | otherwise = do

        let !w = (unsafeShiftL a 35
              .|. unsafeShiftL b 30
              .|. unsafeShiftL c 25
              .|. unsafeShiftL d 20
              .|. unsafeShiftL e 15
              .|. unsafeShiftL f 10
              .|. unsafeShiftL g 5
              .|. h) :: Word64

        poke @Word32 (castPtr dst) (byteSwap32 (fromIntegral (unsafeShiftR w 8)))
        poke @Word8 (plusPtr dst 4) (fromIntegral w)
        go (plusPtr dst 5) (plusPtr src 8)
