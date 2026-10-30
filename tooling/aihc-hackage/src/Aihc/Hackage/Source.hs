-- |
-- Module      : Aihc.Hackage.Source
-- Description : The Hackage releases that a plan can use
--
-- The planner reads Hackage through this record and does not use the
-- network itself. The @aihc-hackage-fetch@ package supplies a record that
-- queries Hackage for one package at a time and downloads releases. A build
-- without that package has no record, and then the plan uses only local
-- packages.
module Aihc.Hackage.Source
  ( HackageSource (..),
    HackageRelease (..),
  )
where

import Aihc.Hackage.Package (Version)
import Aihc.Hackage.Types (PackageSpec)
import Data.ByteString qualified as BS
import Data.Int (Int64)

-- | One version of a package on Hackage.
--
-- The revision of its cabal file is not here: Hackage lists the versions of
-- a package without them, and learning the newest revision of a version
-- costs a request for its cabal file. The planner asks for the newest
-- revision of the version it chooses, and 'hackageCabalFile' says which
-- revision that turned out to be.
data HackageRelease = HackageRelease
  { hackageReleaseVersion :: !Version,
    -- | The maintainer's @preferred-versions@ exclude this version.
    hackageReleaseDeprecated :: !Bool
  }
  deriving (Eq, Show)

-- | The questions that the planner asks of Hackage.
data HackageSource = HackageSource
  { -- | The versions of a package, or 'Nothing' for an unknown package.
    hackageReleases :: String -> IO (Maybe [HackageRelease]),
    -- | The cabal file of one version at one revision, or at the newest
    -- revision when none is named, with the revision that it read.
    hackageCabalFile :: String -> Version -> Maybe Int -> IO (Either String (Int, BS.ByteString)),
    -- | The moment the answers describe, in Unix seconds.
    hackageIndexState :: IO Int64,
    -- | The directory of the unpacked source of a release.
    hackageDownload :: PackageSpec -> IO FilePath
  }
