-- |
-- Module      : Aihc.Hackage.Fetch
-- Description : A Hackage source that queries Hackage and downloads releases
module Aihc.Hackage.Fetch
  ( newHackageSource,
    hackageSourceFor,
  )
where

import Aihc.Hackage.Download (DownloadOptions (..), defaultDownloadOptions, downloadPackageWithOptions)
import Aihc.Hackage.Query (HackageQueries, QueryOptions (..), QueryVersion (..), defaultQueryOptions, newHackageQueries, queryCabalFile, queryPackageVersions)
import Aihc.Hackage.Source (HackageRelease (..), HackageSource (..))
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | Query Hackage with the default options, and show the progress
-- messages and the warnings with the function. Nothing is asked until the
-- planner has a question, so a plan without Hackage packages makes no
-- request.
newHackageSource :: (String -> IO ()) -> IO HackageSource
newHackageSource message =
  hackageSourceWith defaultDownloadOptions {downloadMessage = message}
    <$> newHackageQueries defaultQueryOptions {queryMessage = message}

-- | Answer the questions of the planner with queries.
hackageSourceFor :: HackageQueries -> HackageSource
hackageSourceFor = hackageSourceWith defaultDownloadOptions

hackageSourceWith :: DownloadOptions -> HackageQueries -> HackageSource
hackageSourceWith downloadOptions queries =
  HackageSource
    { hackageReleases = fmap (fmap (map release)) . queryPackageVersions queries,
      hackageCabalFile = queryCabalFile queries,
      -- The answers are as new as the moment they were asked for, since
      -- each one is fetched or revalidated as the plan needs it.
      hackageIndexState = round <$> getPOSIXTime,
      hackageDownload = downloadPackageWithOptions downloadOptions
    }
  where
    release version =
      HackageRelease
        { hackageReleaseVersion = queryVersionVersion version,
          hackageReleaseDeprecated = queryVersionDeprecated version
        }
