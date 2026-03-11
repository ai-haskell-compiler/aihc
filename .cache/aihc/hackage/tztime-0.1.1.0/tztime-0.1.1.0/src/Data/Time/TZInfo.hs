-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZInfo
  ( TZInfo(..)
  , TZIdentifier
  , utc
  -- * System's time zone database
  , loadFromSystem
  , loadFromFile
  , getCurrentTZInfo
  -- * Embedded time zone database
  , fromIdentifier
  , fromLabel
  -- ** TZLabel
  -- $tzlabel
  , TZ.TZLabel(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Zones (TZ)
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as TZ
import GHC.Generics (Generic)
import System.Directory (getSymbolicLinkTarget)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative)

{- | A time zone.

There are two main ways of loading a `TZInfo`:

1. Load it from the operating system's time zone database, using `loadFromSystem`, `loadFromFile`
   or `getCurrentTZInfo`.

2. Load it from the embedded database, using `fromIdentifier` or `fromLabel`.

    This package depends on the @tzdata@ package, which comes with an
    embedded [IANA](https://www.iana.org/time-zones) time zone database.

The embedded database has the benefit of being portable, that is, it works regardless
of your operating system.
The functions to read from the system database, on the other hand, aren't portable;
`loadFromSystem` and `getCurrentTZInfo` are not likely to work on Windows.

However, you have to make sure you're always using the latest version of @tzdata@
to get the latest updates.
The operating system's time zone database is usually easier to keep up-to-date.

-}
data TZInfo = TZInfo
  { tziIdentifier :: TZIdentifier
    -- ^ The time zone's identifier, e.g. @Europe/Paris@.
  , tziRules :: TZ
    -- ^ The time zone's rules describing offset changes.
  }
  deriving stock (Eq, Show, Data, Generic)
  deriving anyclass NFData

-- | A time zone's identifier, e.g. @Europe/Paris@.
type TZIdentifier = Text

-- | The UTC time zone.
utc :: TZInfo
utc = TZInfo "UTC" TZ.utcTZ

----------------------------------------------------------------------------
-- System's time zone database
----------------------------------------------------------------------------

-- | Looks for the time zone file in the system time zone directory, which is
-- @\/usr\/share\/zoneinfo@, or if the @TZDIR@ environment variable is
-- set, then there.
--
-- Note, this is unlikely to work on non-posix systems (e.g.,
-- Windows).
-- Use `fromIdentifier`, `fromLabel` or `loadFromFile` instead.
--
-- Throws an `Control.Exception.IOException` if the identifier is not found.
loadFromSystem :: TZIdentifier -> IO TZInfo
loadFromSystem ident =
  TZInfo ident <$> TZ.loadSystemTZ (T.unpack ident)

-- | Reads and parses a time zone information file (in @tzfile(5)@
-- aka. Olson file format).
loadFromFile :: TZIdentifier -> FilePath -> IO TZInfo
loadFromFile ident filepath =
  TZInfo ident <$> TZ.loadTZFromFile filepath

-- | Returns the local `TZInfo` based on the @TZ@ and @TZDIR@
-- environment variables.
--
-- See @tzset(3)@ for details, but basically:
--
-- * If @TZ@ environment variable is unset, we use @\/etc\/localtime@.
-- * If @TZ@ is set, but empty, we use `utc`.
-- * If @TZ@ is set and not empty, we use `loadFromSystem` to read that file.
getCurrentTZInfo :: IO TZInfo
getCurrentTZInfo =
  lookupEnv "TZ" >>= \case
    Nothing -> do
      filePath <- getSymbolicLinkTarget "/etc/localtime"
      let ident = fromString @TZIdentifier $ makeRelative "/usr/share/zoneinfo" filePath
      tz <- TZ.loadTZFromFile filePath
      pure $ TZInfo ident tz
    Just "" -> pure utc
    Just ident -> TZInfo (fromString ident) <$> TZ.loadSystemTZ ident

----------------------------------------------------------------------------
-- Embedded time zone database
----------------------------------------------------------------------------

-- | Look up a time zone in the @tzdata@'s embedded database.
fromIdentifier :: TZIdentifier -> Maybe TZInfo
fromIdentifier ident =
  TZInfo ident <$> TZ.tzByName (T.encodeUtf8 ident)

-- | Retrieves the time zone info for a "canonical" time zone
-- from @tzdata@'s embedded database.
fromLabel :: TZLabel -> TZInfo
fromLabel label =
  TZInfo
    (T.decodeUtf8 $ TZ.toTZName label)
    (TZ.tzByLabel label)

{- $tzlabel

`TZLabel` enumerates all the "canonical" time zones from the IANA database.

For example, the @2022a@ version of the IANA database defines @Europe/London@ as a
"canonical" time zone and @Europe/Jersey@, @Europe/Guernsey@ and @Europe/Isle_of_Man@ as
links to @Europe/London@.

@
Zone	Europe\/London	-0:01:15 -	LMT	1847 Dec  1  0:00s
			 ...
Link	Europe\/London	Europe\/Jersey
Link	Europe\/London	Europe\/Guernsey
Link	Europe\/London	Europe\/Isle_of_Man
@

Note that `fromLabel` only supports canonical time zone identifiers, whereas
`fromIdentifier` supports all time zone identifiers.

-}
