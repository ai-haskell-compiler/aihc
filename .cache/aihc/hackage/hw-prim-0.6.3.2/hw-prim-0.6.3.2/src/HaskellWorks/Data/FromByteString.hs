{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.FromByteString
  ( FromByteString(..)
  ) where

import Data.Bits
import Data.Word
import Foreign.ForeignPtr

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS
import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

-- | Class for byte-string-like datastructures
class FromByteString a where
  -- | Convert a byte string to a value of type @a
  fromByteString :: BS.ByteString -> a

instance FromByteString (DVS.Vector Word8) where
  fromByteString :: BS.ByteString -> DVS.Vector Word8
  fromByteString bs = DVS.unfoldrN (BS.length bs) gen bs
    where gen :: BS.ByteString -> Maybe (Word8, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) -> Just (d, ds)
            Nothing      -> Nothing
  {-# INLINE fromByteString #-}

instance FromByteString (DVS.Vector Word16) where
  fromByteString :: BS.ByteString -> DVS.Vector Word16
  fromByteString bs = DVS.unfoldrN (BS.length bs `div` 2 + 2) gen bs
    where gen :: BS.ByteString -> Maybe (Word16, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) -> gen' 8 (fromIntegral d) ds
            Nothing      -> Nothing
          gen' :: Int -> Word16 -> BS.ByteString -> Maybe (Word16, BS.ByteString)
          gen' n w cs
            | n >= 16   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) -> gen' (n + 8) (w .|. (fromIntegral d `shiftL` fromIntegral n)) ds
                Nothing      -> Just (w, cs)
  {-# INLINE fromByteString #-}

instance FromByteString (DVS.Vector Word32) where
  fromByteString :: BS.ByteString -> DVS.Vector Word32
  fromByteString bs = DVS.unfoldrN (BS.length bs `div` 4 + 4) gen bs
    where gen :: BS.ByteString -> Maybe (Word32, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) -> gen' 8 (fromIntegral d) ds
            Nothing      -> Nothing
          gen' :: Int -> Word32 -> BS.ByteString -> Maybe (Word32, BS.ByteString)
          gen' n w cs
            | n >= 32   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) -> gen' (n + 8) (w .|. (fromIntegral d `shiftL` fromIntegral n)) ds
                Nothing      -> Just (w, cs)
  {-# INLINE fromByteString #-}

instance FromByteString (DVS.Vector Word64) where
  fromByteString :: BS.ByteString -> DVS.Vector Word64
  fromByteString bs = DVS.unfoldrN (BS.length bs `div` 8 + 8) gen bs
    where gen :: BS.ByteString -> Maybe (Word64, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) -> gen' 8 (fromIntegral d) ds
            Nothing      -> Nothing
          gen' :: Int -> Word64 -> BS.ByteString -> Maybe (Word64, BS.ByteString)
          gen' n w cs
            | n >= 64   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) -> gen' (n + 8) (w .|. (fromIntegral d `shiftL` fromIntegral n)) ds
                Nothing      -> Just (w, cs)
  {-# INLINE fromByteString #-}

instance FromByteString (DVSM.MVector s Word8) where
  fromByteString bs = case BS.toForeignPtr bs of
    (fptr, off, len) -> DVSM.unsafeFromForeignPtr (castForeignPtr fptr) off len
  {-# INLINE fromByteString #-}
