{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Olson.Types
-- Copyright   :  Yitzchak Gale 2019
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- Data types to represent timezone data used in Olson timezone files,
-- as specified by RFC 8536.
--
-- Both Version 1, 2, and 3 timezone data can be represented.

{- Copyright (c) 2019 Yitzchak Gale. All rights reserved.
For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file. -}

module Data.Time.LocalTime.TimeZone.Olson.Types
(
 -- * Olson timezone datatypes
 OlsonData(..),
 Transition(..),
 TransitionType(..),
 TtInfo(..),
 LeapInfo(..),

 -- ** Size limits for Olson timezone data
 SizeLimits(..),
 defaultLimits,
 limitsNoSolar,
 noLimits
)
where

#if MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Monoid (Monoid(..))
import Control.Monad (mplus)

-- | @OlsonData@ represents a full set of timezone data for a location.
--
-- @OlsonData@ can represent timezone data from files in Version 1, 2,
-- or 3 format.
--
-- Version 1 format files can only contain timestamp values that can
-- be represented in less than 32 bits, and cannot contain a POSIX TZ
-- string.
--
-- In a Version 2 format file, the timezone data is split into two
-- parts.  The first part contains timezone data for which all
-- timestamp values can be represented in less than 32 bits, and the
-- second part contains timezone data for which 32 bits or more are
-- required to represent timestamp values. The POSIX TZ string, if
-- present, can only be rendered in a Version 2 file, and appears
-- after both parts of the timezone data.
--
-- Version 3 format files relax certain syntax requirements for the
-- POSIX TZ string.  Since we represent the POSIX TZ string as an
-- unparsed 'String', Version 3 is identical to Version 2 for our
-- purposes.
data OlsonData =
    OlsonData {
      olsonTransitions :: [Transition],
      olsonTypes ::       [TtInfo String],
      olsonLeaps ::       [LeapInfo],
      olsonPosixTZ ::     (Maybe String) 
                            -- ^ Optional POSIX TZ string for
                            -- times after the last @Transition@
    }
  deriving (Eq, Show)

#if MIN_VERSION_base(4,11,0)
instance Semigroup OlsonData where
  OlsonData a  b  c  d <> OlsonData a' b' c' d' =
      OlsonData (a ++ map (shiftBy $ length b) a')
                (b ++ b') (c ++ c') (d `mplus` d')
    where
      shiftBy n trans = trans {transIndex = n + transIndex trans}

instance Monoid OlsonData where
  mempty = OlsonData [] [] [] Nothing
#else
instance Monoid OlsonData where
  mempty = OlsonData [] [] [] Nothing
  mappend (OlsonData a  b  c  d ) (OlsonData a' b' c' d') =
      OlsonData (a ++ map (shiftBy $ length b) a')
                (b ++ b') (c ++ c') (d `mplus` d')
    where
      shiftBy n trans = trans {transIndex = n + transIndex trans}
#endif

-- | A @Transition@ represents a moment when the clocks change in a
-- timezone. It consists of a Unix timestamp value that indicates the
-- exact moment in UTC when the clocks change, and the 0-based index
-- in the list of @TtInfo@ specifications for the description of the
-- new time after the clocks change.
data Transition =
       Transition
         {transTime :: Integer, -- ^ Unix timestamp indicating the time
                                -- when the clocks change
          transIndex :: Int     -- ^ 0-based index in the list of @TtInfo@
                                -- that describes the new time
         }
  deriving (Eq, Show)

-- | A @TransitionType@ is historical information about whether the
-- official body that announced a time change specified the time of
-- the change in terms of UTC, standard time (i.e., non-summer time)
-- for the time zone, or the current wall clock time in the time
-- zone. This historical trivia may seem rather boring, but
-- unfortunately it is needed to interpret a POSIX-style TZ string
-- timezone specification correctly.
data TransitionType = Std | Wall | UTC
  deriving (Eq, Ord, Show)

-- | A @TtInfo@ is a specification of local time in a timezone for
-- some period during which the clocks did not change. `abbr` is
-- @String@ if the timezone abbreviation is represented as a @String@,
-- or @Int@ if it is represented as an index into a long string of
-- null-terminated abbreviation strings (as in an Olson binary
-- timezone file).
data TtInfo abbr = 
       TtInfo
         {tt_utoff :: Int,  -- ^ The offset of local clocks from UTC,
                            -- in seconds
          tt_isdst :: Bool, -- ^ True if local clocks are summer time
          tt_ttype :: TransitionType,
          tt_abbr :: abbr   -- ^ The timezone abbreviation string.
         }
  deriving (Eq, Ord, Show)

-- | Olson timezone files can contain leap second specifications, though
-- most do not.
data LeapInfo =
       LeapInfo
         {leapTime :: Integer, -- ^ A Unix timestamp indicating the time
                               -- that the leap second occurred
          leapOffset :: Int    -- ^ The new total offset of UTC from UT1
                               -- after this leap second
         }
  deriving (Eq, Show)

-- | The reference C implentation imposes size limits on the data
-- structures in a timezone file.
data SizeLimits = SizeLimits
       {maxTimes :: Maybe Int,     -- ^ The maximum number of transition times
        maxTypes :: Maybe Int,     -- ^ The maximum number of TtInfo
                                   -- clock settings
        maxAbbrChars :: Maybe Int, -- ^ The maximum total number of bytes in
                                   -- all timezone abbreviations.
        maxLeaps :: Maybe Int      -- ^ The maximum number of leap second
                                   -- specifications.
       }

-- | The size limits in @defaultLimits@ are taken from the file
-- tzfile.h from tzcode version 2010f. These are the limits for the C
-- implementation on many platforms.
defaultLimits :: SizeLimits
defaultLimits = SizeLimits { maxTimes = Just 1200, maxTypes = Just 256,
                             maxAbbrChars = Just 50, maxLeaps = Just 50 }

-- | @limitsNoSolar@ contains the tighter size limits imposed on some
-- platforms that do not allow timezones that are based on solar time.
limitsNoSolar :: SizeLimits
limitsNoSolar = defaultLimits { maxTypes = Just 20 }

-- | @noLimits@ imposes no size limits. If you use @noLimits@ when
-- parsing, you may exhaust all available memory when reading a faulty
-- or malicious timezone file. If you use @noLimits@ when rendering,
-- the rendered timezone file might not be readable on some systems.
noLimits :: SizeLimits
noLimits = SizeLimits Nothing Nothing Nothing Nothing
