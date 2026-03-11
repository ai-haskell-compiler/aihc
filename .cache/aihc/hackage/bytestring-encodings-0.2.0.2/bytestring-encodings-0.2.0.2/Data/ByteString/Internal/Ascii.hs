--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -O2 #-}

--------------------------------------------------------------------------------

module Data.ByteString.Internal.Ascii
  ( isAscii
  ) where

--------------------------------------------------------------------------------

import Data.Bits ((.&.))
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import Data.Word (Word8, Word64)
import GHC.Base
import GHC.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peek)

import qualified Data.ByteString as B

--------------------------------------------------------------------------------

-- | Binary:  1000000010000000100000001000000010000000100000001000000010000000
--   Decimal: 9259542123273814144
m64 :: Word64
{-# inline m64 #-}
m64 = 0x8080808080808080

-- | Binary:  10000000
--   Decimal: 128
m8 :: Word8
{-# inline m8 #-}
m8 = 0x80

--------------------------------------------------------------------------------

isAsciiPtrW64 :: Ptr Word64 -> Ptr Word64 -> IO Bool
isAsciiPtrW64 !p !q
  | p == q    = pure True
  | otherwise = do
      c <- peek p
      if isAsciiW64 c
        then isAsciiPtrW64 (p `plusPtr` 8) q
        else pure False

isAsciiPtrW8 :: Ptr Word8 -> Ptr Word8 -> IO Bool
isAsciiPtrW8 !p !q
  | p == q    = pure True
  | otherwise = do
      c <- peek p
      if isAsciiW8 c
        then isAsciiPtrW8 (p `plusPtr` 1) q
        else pure False

--------------------------------------------------------------------------------

isAsciiW64 :: Word64 -> Bool
{-# inline isAsciiW64 #-}
isAsciiW64 !w = w .&. m64 == 0

isAsciiW8 :: Word8 -> Bool
{-# inline isAsciiW8 #-}
isAsciiW8 !w = w .&. m8 == 0

isAsciiSmall :: ByteString -> Bool
{-# inline isAsciiSmall #-}
isAsciiSmall !b = B.all (< m8) b

--------------------------------------------------------------------------------

alignPtrPos :: Ptr a -> Ptr a
{-# inline alignPtrPos #-}
alignPtrPos addr@(Ptr a) 
  = case remAddr# a 8# of
      0# -> addr
      n  -> Ptr (plusAddr# a (8# -# n))

alignPtrNeg :: Ptr a -> Ptr a
{-# inline alignPtrNeg #-}
alignPtrNeg addr@(Ptr a)
  = case remAddr# a 8# of
      0# -> addr
      n  -> Ptr (plusAddr# a (negateInt# n))

--------------------------------------------------------------------------------

-- | /O(n\/8)/ detremine if a 'ByteString' is
--   ASCII-encoded.
isAscii :: ByteString -> Bool
{-# inline isAscii #-}
isAscii   (PS _ _ 0)  = True
isAscii b@(PS fp (I# o#) len@(I# l#)) =
  accursedUnutterablePerformIO
    $ withForeignPtr fp
      $ \(Ptr addr) ->
        if len < 8
          then pure (isAsciiSmall b)
          else do
            let
              startPre, endPre, startPost, endPost :: Ptr Word8
              startMid, endMid :: Ptr Word64
            
              startPre  = Ptr (plusAddr# addr o#)
              endPre    = alignPtrPos startPre
              startMid  = castPtr endPre
              endMid    = castPtr startPost
              startPost = alignPtrNeg endPost
              endPost   = Ptr (plusAddr# addr (o# +# l#))
          
            startIsAscii <- isAsciiPtrW8 startPre endPre 
            if startIsAscii
              then do
                endIsAscii <- isAsciiPtrW8 startPost endPost
                if endIsAscii
                  then isAsciiPtrW64 startMid endMid
                  else pure False
              else pure False

--------------------------------------------------------------------------------
