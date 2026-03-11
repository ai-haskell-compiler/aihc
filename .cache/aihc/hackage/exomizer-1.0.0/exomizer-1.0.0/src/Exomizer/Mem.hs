-- | Cruncher interface @exomizer mem@ style.

module Exomizer.Mem
  ( -- * Options & Info
    CrunchOptions
  , coEncoding
  , coMaxPasses
  , coMaxLen
  , coMaxOffset
  , coUseLiteralSequences
  , coFavourSpeed
  , defaultCrunchOptions
  , validCrunchOptions
  , CrunchInfo
  , ciLiteralSequencesUsed
  , ciNeededSafetyOffset
  , ciUsedEncoding
    -- * Direction
  , Direction(..)
  , ReadWrite(..)
  , forward
  , backwards
    -- * Mem crunchers
  , memCrunch
  , memCrunch_
    -- * Join chunks
  , joinChunks
  ) where

import           Data.Bits
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Int
import           Data.List               (foldl1')
import           Data.Word

import           Exomizer.Data.Args
import           Exomizer.Data.Direction
import           Exomizer.Extern.Crunch

-- | Crunch data @exomizer mem@ style. This function does not have automatic joining of multiple chunks, see 'joinChunks'.
--
-- @since 1.0.0
memCrunch :: ReadWrite -> CrunchOptions -> (Int16, ByteString) -> (ByteString, CrunchInfo)
{-# INLINABLE memCrunch #-}
memCrunch (Forward `ReadWrite` Forward) co (la, bs) =
  let
    (bs', ci) = crunchBackwards co (BS.reverse bs)
  in
    (BS.pack [fromIntegral (la `shiftR` 8), fromIntegral la] `BS.append` BS.reverse bs', ci)
memCrunch (Backwards `ReadWrite` Forward) co (la, bs) =
  let
    (bs', ci) = crunchBackwards co (BS.reverse bs)
  in
    (bs' `BS.append` BS.pack [fromIntegral la, fromIntegral (la `shiftR` 8)], ci)
memCrunch (Forward `ReadWrite` Backwards) co (la, bs) =
  let
    (bs', ci) = crunchBackwards co bs
    la' = la + fromIntegral (BS.length bs)
  in
    (BS.pack [fromIntegral (la' `shiftR` 8), fromIntegral la'] `BS.append` BS.reverse bs', ci)
memCrunch (Backwards `ReadWrite` Backwards) co (la, bs) =
  let
    (bs', ci) = crunchBackwards co bs
    la' = la + fromIntegral (BS.length bs)
  in
    (bs' `BS.append` BS.pack [fromIntegral la', fromIntegral (la' `shiftR` 8)], ci)

-- | Identical to 'memCrunch' but does not return the 'CrunchInfo'.
--
-- @since 1.0.0
memCrunch_ :: ReadWrite -> CrunchOptions -> (Int16, ByteString) -> ByteString
{-# INLINABLE memCrunch_ #-}
memCrunch_ readWrite args = fst . memCrunch readWrite args

-- | Join the supplied chunks into one. The spaces in between are filled with the supplied byte.
--
--   There is no check for overlap or if the result exceeds the memory space.
--
-- @since 1.0.0
joinChunks :: Word8 -> [(Int16, ByteString)] -> (Int16, ByteString)
joinChunks _    []     = (0, BS.empty)
joinChunks fill chunks = foldl1' combine chunks
  where
    combine :: (Int16, ByteString) -> (Int16, ByteString) -> (Int16, ByteString)
    combine (la, ba) (lb, bb)
      | la > lb =
          combine (lb, bb) (la, ba)
      | la' + lba >= lb' =
          (la, BS.take (lb' - la') ba `BS.append` bb)
      | otherwise =
          (la, ba `BS.append` BS.replicate (lb' - (la' + lba)) fill `BS.append` bb)
      where
        la' = fromIntegral la
        lb' = fromIntegral lb
        lba = BS.length ba
