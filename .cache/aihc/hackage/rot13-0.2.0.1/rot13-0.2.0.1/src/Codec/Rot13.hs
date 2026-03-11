{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Codec.Rot13
-- Description : Fast ROT13 cipher for Haskell.
-- Copyright   : (c) Kyle Van Berendonck, 2014
-- License     : BSD3
-- Maintainer  : kvanberendonck@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module exposes the API for this package.
module Codec.Rot13
  ( -- * Typeclass Interfaces
    Rot13(..)
  , Rot13Bytes(..)

    -- * Constraint Interfaces
  , rot13enum
  , rot13int

    -- * Compatibility
  , rot13word
  , rot13word8
  , rot13char
  , rot13string
  ) where

import           Data.Char
import           Data.Word
import           Data.Int
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Internal   as BS
import qualified Data.Text                  as Text
import           Foreign.Ptr
import           Foreign.Storable
import qualified Foreign.C.Types            as Foreign


-- | The 'Rot13' typeclass is intended to perform the ROT13 cipher on the provided data, as if it
--   were representing a single ANSI-encoded character. This interface doesn't consider the storage
--   behaviour of the type at all, but is the fastest implementation if you need to integrate the
--   transformation as part of a stream.

class Rot13 a where
  rot13 :: a -> a


-- | The 'Rot13Bytes' typeclass is intended for when you need to perform the ROT13 cipher on some
--   data at the memory level. It stores the given data into a temporary buffer in memory, then runs
--   the cipher over the stored bytes to produce a new buffer. This operation is typically slower
--   than just using 'rot13' as part of a fusion pipeline.

class Rot13Bytes a where
  rot13bs :: a -> BS.ByteString


-- | Perform the ROT13 cipher on the given 'Integral' instance (in the sense of 'Rot13').
rot13int :: Integral a => a -> a
rot13int x
  | (fromIntegral x :: Word) - 97 < 26 = 97 + rem (x - 84) 26
  | (fromIntegral x :: Word) - 65 < 26 = 65 + rem (x - 52) 26
  | otherwise   = x
{-# INLINE rot13int #-}
{-# SPECIALIZE rot13int :: Word -> Word #-}
{-# SPECIALIZE rot13int :: Word8 -> Word8 #-}
{-# SPECIALIZE rot13int :: Word16 -> Word16 #-}
{-# SPECIALIZE rot13int :: Word32 -> Word32 #-}
{-# SPECIALIZE rot13int :: Word64 -> Word64 #-}
{-# SPECIALIZE rot13int :: Int -> Int #-}
{-# SPECIALIZE rot13int :: Int8 -> Int8 #-}
{-# SPECIALIZE rot13int :: Int16 -> Int16 #-}
{-# SPECIALIZE rot13int :: Int32 -> Int32 #-}
{-# SPECIALIZE rot13int :: Int64 -> Int64 #-}
{-# SPECIALIZE rot13int :: Integer -> Integer #-}
{-# SPECIALIZE rot13int :: Foreign.CChar -> Foreign.CChar #-}
{-# SPECIALIZE rot13int :: Foreign.CSChar -> Foreign.CSChar #-}
{-# SPECIALIZE rot13int :: Foreign.CUChar -> Foreign.CUChar #-}
{-# SPECIALIZE rot13int :: Foreign.CShort -> Foreign.CShort #-}
{-# SPECIALIZE rot13int :: Foreign.CUShort -> Foreign.CUShort #-}
{-# SPECIALIZE rot13int :: Foreign.CInt -> Foreign.CInt #-}
{-# SPECIALIZE rot13int :: Foreign.CUInt -> Foreign.CUInt #-}
{-# SPECIALIZE rot13int :: Foreign.CLong -> Foreign.CLong #-}
{-# SPECIALIZE rot13int :: Foreign.CULong -> Foreign.CULong #-}
{-# SPECIALIZE rot13int :: Foreign.CWchar -> Foreign.CWchar #-}
{-# SPECIALIZE rot13int :: Foreign.CLLong -> Foreign.CLLong #-}
{-# SPECIALIZE rot13int :: Foreign.CULLong -> Foreign.CULLong #-}

-- | Perform the ROT13 cipher on the given 'Enum' instance (in the sense of 'Rot13').
{-# INLINE rot13enum #-}
rot13enum :: Enum a => a -> a
rot13enum = toEnum . (rot13int :: Int -> Int) . fromEnum

-- | Perform the ROT13 cipher on the given 'Storable' instance bytes to yield a 'BS.ByteString'.
{-# INLINE rot13stor #-}
rot13stor :: Storable a => a -> BS.ByteString
rot13stor x = rot13bs $! BS.unsafeCreate (sizeOf x) $ \ptr -> poke (castPtr ptr) x


--------------------------------------------------------------------------------------------------
-- Rot13 Instances

instance Rot13 Char                   where rot13 = rot13enum
instance Rot13 String                 where rot13 = map rot13
instance Rot13 BS.ByteString          where rot13 = BS.map rot13
instance Rot13 Text.Text              where rot13 = Text.map rot13

instance Rot13 Word                   where rot13 = rot13int
instance Rot13 Word8                  where rot13 = rot13int
instance Rot13 Word16                 where rot13 = rot13int
instance Rot13 Word32                 where rot13 = rot13int
instance Rot13 Word64                 where rot13 = rot13int

instance Rot13 Int                    where rot13 = rot13int
instance Rot13 Int8                   where rot13 = rot13int
instance Rot13 Int16                  where rot13 = rot13int
instance Rot13 Int32                  where rot13 = rot13int
instance Rot13 Int64                  where rot13 = rot13int
instance Rot13 Integer                where rot13 = rot13int

instance Rot13 Foreign.CChar          where rot13 = rot13
instance Rot13 Foreign.CSChar         where rot13 = rot13
instance Rot13 Foreign.CUChar         where rot13 = rot13
instance Rot13 Foreign.CShort         where rot13 = rot13
instance Rot13 Foreign.CUShort        where rot13 = rot13
instance Rot13 Foreign.CInt           where rot13 = rot13
instance Rot13 Foreign.CUInt          where rot13 = rot13
instance Rot13 Foreign.CLong          where rot13 = rot13
instance Rot13 Foreign.CULong         where rot13 = rot13
instance Rot13 Foreign.CWchar         where rot13 = rot13
instance Rot13 Foreign.CLLong         where rot13 = rot13
instance Rot13 Foreign.CULLong        where rot13 = rot13

--------------------------------------------------------------------------------------------------
-- Rot13Bytes Instances

instance {-# OVERLAPPING #-} Rot13Bytes BS.ByteString where rot13bs = rot13
instance {-# OVERLAPPING #-} Storable a => Rot13Bytes a where rot13bs = rot13stor


--------------------------------------------------------------------------------------------------
-- Compatibility

{-# INLINE rot13word #-}
rot13word :: Word -> Word
rot13word = rot13

{-# INLINE rot13word8 #-}
rot13word8 :: Word8 -> Word8
rot13word8 = rot13

{-# INLINE rot13char #-}
rot13char :: Char -> Char
rot13char = rot13

{-# INLINE rot13string #-}
rot13string :: String -> String
rot13string = rot13
