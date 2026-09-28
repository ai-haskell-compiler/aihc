-- |
-- Module      : Aihc.Hackage.Fetch
-- Description : A Hackage source that reads the index and downloads releases
module Aihc.Hackage.Fetch
  ( newHackageSource,
    hackageSourceFor,
  )
where

import Aihc.Hackage.Download (defaultDownloadOptions, downloadPackageWithOptions)
import Aihc.Hackage.Index (IndexEntry (..))
import Aihc.Hackage.IndexCache (HackageIndex, IndexVersion (..), defaultIndexOptions, indexPackageVersions, indexReadCabalFile, indexState, newHackageIndex)
import Aihc.Hackage.Source (HackageRelease (..), HackageSource (..))

-- | The cached Hackage index with the default options. The index is read on
-- the first question, so a plan without Hackage packages does not read it.
newHackageSource :: IO HackageSource
newHackageSource = hackageSourceFor <$> newHackageIndex defaultIndexOptions

-- | Answer the questions of the planner from an index.
hackageSourceFor :: HackageIndex -> HackageSource
hackageSourceFor index =
  HackageSource
    { hackageReleases = fmap (fmap (map release)) . indexPackageVersions index,
      hackageCabalFile = indexReadCabalFile index,
      hackageIndexState = indexState index,
      hackageDownload = downloadPackageWithOptions defaultDownloadOptions
    }
  where
    release version =
      HackageRelease
        { hackageReleaseVersion = indexVersionVersion version,
          hackageReleaseRevisions = map indexEntryRevision (indexVersionRevisions version),
          hackageReleaseDeprecated = indexVersionDeprecated version
        }
