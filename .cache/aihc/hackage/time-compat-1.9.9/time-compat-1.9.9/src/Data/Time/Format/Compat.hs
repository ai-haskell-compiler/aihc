{-# LANGUAGE CPP #-}
module Data.Time.Format.Compat (
    -- * UNIX-style formatting
    FormatTime(),formatTime,

    -- * UNIX-style parsing
    -- ** __Note__ in compat mode acceptWS argument is ignored, it's always 'True'.
    parseTimeM,
    -- parseTimeMultipleM, -- TODO
    parseTimeOrError,
    readSTime, readPTime,
    parseTime, readTime, readsTime,
    ParseTime(),

    -- * Locale
    TimeLocale(..),

    defaultTimeLocale,

    iso8601DateFormat,
    rfc822DateFormat,
    ) where

import Data.Time.Orphans ()

#if !(MIN_VERSION_time(1,9,0))
import Data.Time.Format hiding (parseTimeM)
#else
import Data.Time.Format
#endif

import qualified Control.Monad.Fail as Fail
import qualified Data.Time.Format

-- parseTimeM has always Fail.MonadFail constraint
#if !MIN_VERSION_time(1,9,0)
-- | Parses a time value given a format string.
--
-- This variant from @time-compat@ has always 'Fail.MonadFail' constraint.
--
-- Look at 'Data.Time.Format.parseTimeM' for documentation.
parseTimeM
    :: (Fail.MonadFail m, ParseTime t)
    => Bool       -- ^ Accept leading and trailing whitespace?
    -> TimeLocale -- ^ Time locale.
    -> String     -- ^ Format string.
    -> String     -- ^ Input string.
    -> m t        -- ^ Return the time value, or fail if the in
parseTimeM = Data.Time.Format.parseTimeM
#endif

#if MIN_VERSION_time(1,10,0)
{-# DEPRECATED parseTime "use \"parseTimeM True\" instead" #-}
parseTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string.
          -> String     -- ^ Input string.
          -> Maybe t    -- ^ The time value, or 'Nothing' if the input could
                        -- not be parsed using the given format.
parseTime = parseTimeM True

{-# DEPRECATED readTime "use \"parseTimeOrError True\" instead" #-}
readTime :: ParseTime t =>
            TimeLocale -- ^ Time locale.
         -> String     -- ^ Format string.
         -> String     -- ^ Input string.
         -> t          -- ^ The time value.
readTime = parseTimeOrError True

{-# DEPRECATED readsTime "use \"readSTime True\" instead" #-}
readsTime :: ParseTime t =>
             TimeLocale -- ^ Time locale.
          -> String     -- ^ Format string
          -> ReadS t
readsTime = readSTime True
#endif

-- TODO:
--
-- #if !MIN_VERSION_time(1,11,0)
-- -- | Parses a time value given a list of pairs of format and input.
-- -- Resulting value is constructed from all provided specifiers.
-- parseTimeMultipleM
--     :: (Fail.MonadFail m, ParseTime t)
--     => Bool -- ^ Accept leading and trailing whitespace?
--     -> TimeLocale -- ^ Time locale.
--     -> [(String, String)] -- ^ Pairs of (format string, input string).
--     -> m t -- ^ Return the time value, or fail if the input could not be parsed using the given format.
-- parseTimeMultipleM = undefined -- parseTimeMultipleM' Proxy
-- #endif
