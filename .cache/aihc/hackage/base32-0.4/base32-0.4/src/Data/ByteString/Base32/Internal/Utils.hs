{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
module Data.ByteString.Base32.Internal.Utils
( aix
, peekWord32BE
, peekWord64BE
, reChunkN
, w32
, w64
, w64_32
, writeNPlainPtrBytes
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Foreign.Ptr
import Foreign.Storable

import GHC.ByteOrder
import GHC.Exts
import GHC.Word

import System.IO.Unsafe
import Foreign.Marshal.Alloc (mallocBytes)


-- | Read 'Word8' index off alphabet addr
--
aix :: Word8 -> Addr# -> Word8
aix w8 alpha = W8# (indexWord8OffAddr# alpha i)
  where
    !(I# i) = fromIntegral w8
{-# INLINE aix #-}

w32 :: Word8 -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

w64_32 :: Word32 -> Word64
w64_32 = fromIntegral
{-# INLINE w64_32 #-}

w64 :: Word8 -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

-- | Allocate and fill @n@ bytes with some data
--
writeNPlainPtrBytes
  :: Storable a
  => Int
  -> [a]
  -> Ptr a
writeNPlainPtrBytes !n as = unsafeDupablePerformIO $ do
    p <- mallocBytes n
    go p as
    return p
  where
    go !_ [] = return ()
    go !p (x:xs) = poke p x >> go (plusPtr p 1) xs
{-# INLINE writeNPlainPtrBytes #-}

peekWord32BE :: Ptr Word32 -> IO Word32
peekWord32BE p = case targetByteOrder of
  LittleEndian -> byteSwap32 <$> peek p
  BigEndian -> peek p
{-# inline peekWord32BE #-}

peekWord64BE :: Ptr Word64 -> IO Word64
peekWord64BE p = case targetByteOrder of
  LittleEndian -> byteSwap64 <$> peek p
  BigEndian -> peek p
{-# inline peekWord64BE #-}

-- | Rechunk a list of bytestrings in multiples of @n@
--
reChunkN :: Int -> [ByteString] -> [ByteString]
reChunkN n = go
  where
    go [] = []
    go (b:bs) = case divMod (BS.length b) n of
      (_, 0) -> b : go bs
      (d, _) -> case BS.splitAt (d * n) b of
        ~(h, t) -> h : accum t bs

    accum acc [] = [acc]
    accum acc (c:cs) =
      case BS.splitAt (n - BS.length acc) c of
        ~(h, t) ->
          let acc' = BS.append acc h
          in if BS.length acc' == n
             then
               let cs' = if BS.null t then cs else t : cs
               in acc' : go cs'
             else accum acc' cs
{-# INLINE reChunkN #-}
