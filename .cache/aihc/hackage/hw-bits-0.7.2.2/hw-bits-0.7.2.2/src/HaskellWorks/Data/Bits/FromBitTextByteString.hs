{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Bits.FromBitTextByteString
    ( FromBitTextByteString(..)
    ) where

import Data.Word
import HaskellWorks.Data.Bits

import qualified Data.Bit             as Bit
import qualified Data.Bit.ThreadSafe  as BitTS
import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed  as DVU

class FromBitTextByteString a where
  -- | Convert a binary byte string to a value of type @a
  fromBitTextByteString :: BS.ByteString -> a

instance FromBitTextByteString (DVS.Vector Word8) where
  fromBitTextByteString :: BS.ByteString -> DVS.Vector Word8
  fromBitTextByteString bs = DVS.unfoldrN (BS.length bs `div` 8 + 1) gen bs
    where gen :: BS.ByteString -> Maybe (Word8, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) | d == w0  -> gen' 1 0 ds
            Just (d, ds) | d == w1  -> gen' 1 1 ds
            Just (_, ds) -> gen ds
            Nothing      -> Nothing
          gen' :: Int -> Word8 -> BS.ByteString -> Maybe (Word8, BS.ByteString)
          gen' n w cs
            | n >= 8   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) | d == w0  -> gen' (n + 1) (w .|. (0 .<. fromIntegral n)) ds
                Just (d, ds) | d == w1  -> gen' (n + 1) (w .|. (1 .<. fromIntegral n)) ds
                Just (_, ds) -> gen' n w ds
                Nothing      -> Just (w, cs)

instance FromBitTextByteString (DVS.Vector Word16) where
  fromBitTextByteString :: BS.ByteString -> DVS.Vector Word16
  fromBitTextByteString bs = DVS.unfoldrN (BS.length bs `div` 16 + 1) gen bs
    where gen :: BS.ByteString -> Maybe (Word16, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) | d == w0  -> gen' 1 0 ds
            Just (d, ds) | d == w1  -> gen' 1 1 ds
            Just (_, ds) -> gen ds
            Nothing      -> Nothing
          gen' :: Int -> Word16 -> BS.ByteString -> Maybe (Word16, BS.ByteString)
          gen' n w cs
            | n >= 16   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) | d == w0  -> gen' (n + 1) (w .|. (0 .<. fromIntegral n)) ds
                Just (d, ds) | d == w1  -> gen' (n + 1) (w .|. (1 .<. fromIntegral n)) ds
                Just (_, ds) -> gen' n w ds
                Nothing      -> Just (w, cs)

instance FromBitTextByteString (DVS.Vector Word32) where
  fromBitTextByteString :: BS.ByteString -> DVS.Vector Word32
  fromBitTextByteString bs = DVS.unfoldrN (BS.length bs `div` 32 + 1) gen bs
    where gen :: BS.ByteString -> Maybe (Word32, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) | d == w0  -> gen' 1 0 ds
            Just (d, ds) | d == w1  -> gen' 1 1 ds
            Just (_, ds) -> gen ds
            Nothing      -> Nothing
          gen' :: Int -> Word32 -> BS.ByteString -> Maybe (Word32, BS.ByteString)
          gen' n w cs
            | n >= 32   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) | d == w0  -> gen' (n + 1) (w .|. (0 .<. fromIntegral n)) ds
                Just (d, ds) | d == w1  -> gen' (n + 1) (w .|. (1 .<. fromIntegral n)) ds
                Just (_, ds) -> gen' n w ds
                Nothing      -> Just (w, cs)

instance FromBitTextByteString (DVS.Vector Word64) where
  fromBitTextByteString :: BS.ByteString -> DVS.Vector Word64
  fromBitTextByteString bs = DVS.unfoldrN (BS.length bs `div` 64 + 1) gen bs
    where gen :: BS.ByteString -> Maybe (Word64, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) | d == w0  -> gen' 1 0 ds
            Just (d, ds) | d == w1  -> gen' 1 1 ds
            Just (_, ds) -> gen ds
            Nothing      -> Nothing
          gen' :: Int -> Word64 -> BS.ByteString -> Maybe (Word64, BS.ByteString)
          gen' n w cs
            | n >= 64   = Just (w, cs)
            | otherwise = case BS.uncons cs of
                Just (d, ds) | d == w0  -> gen' (n + 1) (w .|. (0 .<. fromIntegral n)) ds
                Just (d, ds) | d == w1  -> gen' (n + 1) (w .|. (1 .<. fromIntegral n)) ds
                Just (_, ds) -> gen' n w ds
                Nothing      -> Just (w, cs)

instance FromBitTextByteString (DVU.Vector Bit.Bit) where
  fromBitTextByteString :: BS.ByteString -> DVU.Vector Bit.Bit
  fromBitTextByteString bs = DVU.unfoldrN (BS.length bs) gen bs
    where gen :: BS.ByteString -> Maybe (Bit.Bit, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) | d == w0  -> Just (Bit.Bit False, ds)
            Just (d, ds) | d == w1  -> Just (Bit.Bit True,  ds)
            Just (_, ds) -> gen ds
            Nothing      -> Nothing

instance FromBitTextByteString (DVU.Vector BitTS.Bit) where
  fromBitTextByteString :: BS.ByteString -> DVU.Vector BitTS.Bit
  fromBitTextByteString bs = DVU.unfoldrN (BS.length bs) gen bs
    where gen :: BS.ByteString -> Maybe (BitTS.Bit, BS.ByteString)
          gen cs = case BS.uncons cs of
            Just (d, ds) | d == w0  -> Just (BitTS.Bit False, ds)
            Just (d, ds) | d == w1  -> Just (BitTS.Bit True,  ds)
            Just (_, ds) -> gen ds
            Nothing      -> Nothing

w0 :: Word8
w0 = 48

w1 :: Word8
w1 = 49
