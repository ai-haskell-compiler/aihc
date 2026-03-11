-- | The functions of this module are identical to use the command line tool.
--
--   The modules "Exomizer.Raw" and "Exomizer.Mem" give more options.
--
--   Each function is listed with and without an underscore at the end.
--   The variant with the underscore does not return the compression info but the compression result is identical.

module Exomizer
  ( -- * Options & Info
    CrunchOptions
  , coEncoding
  , coMaxPasses
  , coMaxLen
  , coMaxOffset
  , coUseLiteralSequences
  , coFavourSpeed
  , defaultCrunchOptions
  , CrunchInfo
  , ciLiteralSequencesUsed
  , ciNeededSafetyOffset
  , ciUsedEncoding
    -- * Raw
  , exomizerRaw
  , exomizerRaw_
  , exomizerRawBackwards
  , exomizerRawBackwards_
  , exomizerRawReverse
  , exomizerRawReverse_
  , exomizerRawBackwardsReverse
  , exomizerRawBackwardsReverse_
  -- * Mem
  , exomizerMem
  , exomizerMem_
  , exomizerMemForward
  , exomizerMemForward_
  -- * Level
  , exomizerLevel_
  , exomizerLevelForward_
  ) where

import           Data.ByteString (ByteString)
import           Data.Int

import           Exomizer.Mem
import           Exomizer.Raw

-- raw

-- | Same as @exomizer raw@
--
-- @since 1.0.0
exomizerRaw :: CrunchOptions -> ByteString -> (ByteString, CrunchInfo)
{-# INLINABLE exomizerRaw #-}
exomizerRaw = rawCrunch (Forward `ReadWrite` Forward)

-- | Same as @exomizer raw@
--
-- @since 1.0.0
exomizerRaw_ :: CrunchOptions -> ByteString -> ByteString
{-# INLINABLE exomizerRaw_ #-}
exomizerRaw_ = rawCrunch_ (Forward `ReadWrite` Forward)

-- | Same as @exomizer raw -b@
--
-- @since 1.0.0
exomizerRawBackwards :: CrunchOptions -> ByteString -> (ByteString, CrunchInfo)
{-# INLINABLE exomizerRawBackwards #-}
exomizerRawBackwards = rawCrunch (Backwards `ReadWrite` Backwards)

-- | Same as @exomizer raw -b@
--
-- @since 1.0.0
exomizerRawBackwards_ :: CrunchOptions -> ByteString -> ByteString
{-# INLINABLE exomizerRawBackwards_ #-}
exomizerRawBackwards_ = rawCrunch_ (Backwards `ReadWrite` Backwards)

-- | Same as @exomizer raw -r@
--
-- @since 1.0.0
exomizerRawReverse :: CrunchOptions -> ByteString -> (ByteString, CrunchInfo)
{-# INLINABLE exomizerRawReverse #-}
exomizerRawReverse = rawCrunch (Backwards `ReadWrite` Forward)

-- | Same as @exomizer raw -r@
--
-- @since 1.0.0
exomizerRawReverse_ :: CrunchOptions -> ByteString -> ByteString
{-# INLINABLE exomizerRawReverse_ #-}
exomizerRawReverse_ = rawCrunch_ (Backwards `ReadWrite` Forward)

-- | Same as @exomizer raw -b -r@
--
-- @since 1.0.0
exomizerRawBackwardsReverse :: CrunchOptions -> ByteString -> (ByteString, CrunchInfo)
{-# INLINABLE exomizerRawBackwardsReverse #-}
exomizerRawBackwardsReverse = rawCrunch (Forward `ReadWrite` Backwards)

-- | Same as @exomizer raw -b -r@
--
-- @since 1.0.0
exomizerRawBackwardsReverse_ :: CrunchOptions -> ByteString -> ByteString
{-# INLINABLE exomizerRawBackwardsReverse_ #-}
exomizerRawBackwardsReverse_ = rawCrunch_ (Forward `ReadWrite` Backwards)

-- mem

-- | Same as @exomizer mem -l none@
--
-- @since 1.0.0
exomizerMem :: CrunchOptions -> [(Int16, ByteString)] -> (ByteString, CrunchInfo)
{-# INLINABLE exomizerMem #-}
exomizerMem co xs = memCrunch (Backwards `ReadWrite` Backwards) co (joinChunks 0 xs)

-- | Same as @exomizer mem -l none@
--
-- @since 1.0.0
exomizerMem_ :: CrunchOptions -> [(Int16, ByteString)] -> ByteString
{-# INLINABLE exomizerMem_ #-}
exomizerMem_ co xs = memCrunch_ (Backwards `ReadWrite` Backwards) co (joinChunks 0 xs)

-- | Same as @exomizer mem -l none -f@
--
-- @since 1.0.0
exomizerMemForward :: CrunchOptions -> [(Int16, ByteString)] -> (ByteString, CrunchInfo)
{-# INLINABLE exomizerMemForward #-}
exomizerMemForward co xs = memCrunch (Forward `ReadWrite` Forward) co (joinChunks 0 xs)

-- | Same as @exomizer mem -l none -f@
--
-- @since 1.0.0
exomizerMemForward_ :: CrunchOptions -> [(Int16, ByteString)] -> ByteString
{-# INLINABLE exomizerMemForward_ #-}
exomizerMemForward_ co xs = memCrunch_ (Forward `ReadWrite` Forward) co (joinChunks 0 xs)

-- level

-- | Same as @exomizer level@
--
-- @since 1.0.0
exomizerLevel_ :: CrunchOptions -> [(Int16, ByteString)] -> ByteString
{-# INLINABLE exomizerLevel_ #-}
exomizerLevel_ co xs = mconcat $ map (memCrunch_ (Forward `ReadWrite` Backwards) co) xs

-- | Same as @exomizer level -f@
--
-- @since 1.0.0
exomizerLevelForward_ :: CrunchOptions -> [(Int16, ByteString)] -> ByteString
{-# INLINABLE exomizerLevelForward_ #-}
exomizerLevelForward_ co xs = mconcat $ map (memCrunch_ (Forward `ReadWrite` Forward) co) xs
