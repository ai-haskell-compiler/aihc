-- |
-- Module      : Aihc.Hackage.Fetch
-- Description : A Hackage source that queries Hackage and downloads releases
module Aihc.Hackage.Fetch
  ( newHackageSource,
    hackageSourceFor,
  )
where

import Aihc.Hackage.Download (defaultDownloadOptions, downloadPackageWithOptions)
import Aihc.Hackage.Query (HackageQueries, QueryVersion (..), defaultQueryOptions, newHackageQueries, queryCabalFile, queryPackageVersions)
import Aihc.Hackage.Source (HackageRelease (..), HackageSource (..))
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | Query Hackage with the default options. Nothing is asked until the
-- planner has a question, so a plan without Hackage packages makes no
-- request.
newHackageSource :: IO HackageSource
newHackageSource = hackageSourceFor <$> newHackageQueries defaultQueryOptions

-- | Answer the questions of the planner with queries.
hackageSourceFor :: HackageQueries -> HackageSource
hackageSourceFor queries =
  HackageSource
    { hackageReleases = fmap (fmap (map release)) . queryPackageVersions queries,
      hackageCabalFile = queryCabalFile queries,
      -- The answers are as new as the moment they were asked for, since
      -- each one is fetched or revalidated as the plan needs it.
      hackageIndexState = round <$> getPOSIXTime,
      hackageDownload = downloadPackageWithOptions defaultDownloadOptions
    }
  where
    release version =
      HackageRelease
        { hackageReleaseVersion = queryVersionVersion version,
          hackageReleaseDeprecated = queryVersionDeprecated version
        }
