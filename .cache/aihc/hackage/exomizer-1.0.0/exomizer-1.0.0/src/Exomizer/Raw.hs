-- | Cruncher interface @exomizer raw@ style.

module Exomizer.Raw
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
    -- * Raw crunchers
  , rawCrunch
  , rawCrunch_
  , rawDecrunch
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS

import           Exomizer.Data.Args
import           Exomizer.Data.Direction
import           Exomizer.Extern.Crunch

-- | Crunch data @exomizer raw@ style.
--
-- @since 1.0.0
rawCrunch :: ReadWrite -> CrunchOptions -> ByteString -> (ByteString, CrunchInfo)
{-# INLINABLE rawCrunch #-}
rawCrunch (readDir `ReadWrite` writeDir) co bs =
  let
    (bs', ci) = crunchBackwards co (ifForward writeDir BS.reverse bs)
  in
    (ifForward readDir BS.reverse bs', ci)

-- | Identical to 'rawCrunch' but does not return the 'CrunchInfo'.
--
-- @since 1.0.0
rawCrunch_ :: ReadWrite -> CrunchOptions -> ByteString -> ByteString
{-# INLINABLE rawCrunch_ #-}
rawCrunch_ readWrite args = fst . rawCrunch readWrite args

-- | Decrunch some data.
--
--   WARNING: The linked exomizer library (and with that the program) may segfault when the input data is not valid!
--
-- @since 1.0.0
rawDecrunch :: ReadWrite -> ByteString -> ByteString
{-# INLINABLE rawDecrunch #-}
rawDecrunch (readDir `ReadWrite` writeDir) bs =
  let
    bs' = deCrunchForward (ifBackwards readDir BS.reverse bs)
  in
    ifBackwards writeDir BS.reverse bs'
