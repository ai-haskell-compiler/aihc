{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base32.Internal.Tail
( loopTail
, loopTailNoPad
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base32.Internal.Utils

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import GHC.Exts
import GHC.Word


-- | Unroll final quantum encoding for base32
--
loopTail
    :: Addr#
    -> ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ByteString
loopTail !lut !dfp !dptr !end !dst !src
    | src == end = return (BS dfp (minusPtr dst dptr))
    | plusPtr src 1 == end = do -- 2 6
      !a <- peek src

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2)

      poke dst t
      poke (plusPtr dst 1) u
      padN (plusPtr dst 2) 6

      return (BS dfp (8 + minusPtr dst dptr))
    | plusPtr src 2 == end = do -- 4 4
      !a <- peek src
      !b <- peek (plusPtr src 1)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2 .|. unsafeShiftR (b .&. 0xc0) 6)
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      padN (plusPtr dst 4) 4

      return (BS dfp (8 + minusPtr dst dptr))
    | plusPtr src 3 == end = do -- 5 3
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2 .|. unsafeShiftR (b .&. 0xc0) 6)
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4 .|. unsafeShiftR (c .&. 0xf0) 4)
          !x = look (unsafeShiftL (c .&. 0x0f) 1)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      padN (plusPtr dst 5) 3
      return (BS dfp (8 + minusPtr dst dptr))

    | otherwise = do -- 7 1
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)
      !d <- peek (plusPtr src 3)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2 .|. unsafeShiftR (b .&. 0xc0) 6)
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4 .|. unsafeShiftR (c .&. 0xf0) 4)
          !x = look (unsafeShiftL (c .&. 0x0f) 1 .|. unsafeShiftR (d .&. 0x80) 7)
          !y = look (unsafeShiftR (d .&. 0x7c) 2)
          !z = look (unsafeShiftL (d .&. 0x03) 3)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      poke (plusPtr dst 5) y
      poke (plusPtr dst 6) z
      padN (plusPtr dst 7) 1
      return (BS dfp (8 + minusPtr dst dptr))
  where
    look !n = aix n lut

    padN :: Ptr Word8 -> Int -> IO ()
    padN !_ 0 = return ()
    padN !p n = poke p 0x3d >> padN (plusPtr p 1) (n - 1)
{-# INLINE loopTail #-}

-- | Unroll final quantum encoding for base32
--
loopTailNoPad
    :: Addr#
    -> ForeignPtr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> Ptr Word8
    -> IO ByteString
loopTailNoPad !lut !dfp !dptr !end !dst !src
  | src == end = return (BS dfp (minusPtr dst dptr))
  | plusPtr src 1 == end = do -- 2 6
      !a <- peek src

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2)

      poke dst t
      poke (plusPtr dst 1) u

      return (BS dfp (2 + minusPtr dst dptr))

    | plusPtr src 2 == end = do -- 4 4
      !a <- peek src
      !b <- peek (plusPtr src 1)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2 .|. unsafeShiftR (b .&. 0xc0) 6)
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w

      return (BS dfp (4 + minusPtr dst dptr))

    | plusPtr src 3 == end = do -- 5 3
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2 .|. unsafeShiftR (b .&. 0xc0) 6)
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4 .|. unsafeShiftR (c .&. 0xf0) 4)
          !x = look (unsafeShiftL (c .&. 0x0f) 1)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      return (BS dfp (5 + minusPtr dst dptr))

    | otherwise = do -- 7 1
      !a <- peek src
      !b <- peek (plusPtr src 1)
      !c <- peek (plusPtr src 2)
      !d <- peek (plusPtr src 3)

      let !t = look (unsafeShiftR (a .&. 0xf8) 3)
          !u = look (unsafeShiftL (a .&. 0x07) 2 .|. unsafeShiftR (b .&. 0xc0) 6)
          !v = look (unsafeShiftR (b .&. 0x3e) 1)
          !w = look (unsafeShiftL (b .&. 0x01) 4 .|. unsafeShiftR (c .&. 0xf0) 4)
          !x = look (unsafeShiftL (c .&. 0x0f) 1 .|. unsafeShiftR (d .&. 0x80) 7)
          !y = look (unsafeShiftR (d .&. 0x7c) 2)
          !z = look (unsafeShiftL (d .&. 0x03) 3)

      poke dst t
      poke (plusPtr dst 1) u
      poke (plusPtr dst 2) v
      poke (plusPtr dst 3) w
      poke (plusPtr dst 4) x
      poke (plusPtr dst 5) y
      poke (plusPtr dst 6) z
      return (BS dfp (7 + minusPtr dst dptr))
  where
    look !i = aix i lut
{-# INLINE loopTailNoPad #-}
