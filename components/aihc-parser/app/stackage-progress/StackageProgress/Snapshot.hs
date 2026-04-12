-- | Stackage snapshot fetching and parsing.
--
-- Thin re-export layer over "Aihc.Hackage.Stackage".
module StackageProgress.Snapshot
  ( loadStackageSnapshotWithMode,
    parseSnapshotConstraints,
    snapshotCacheFile,
  )
where

import Aihc.Hackage.Cache (snapshotCacheFile)
import Aihc.Hackage.Stackage (loadStackageSnapshot, parseSnapshotConstraints)
import Aihc.Hackage.Types (PackageSpec)

-- | Load a Stackage snapshot, either from cache or by fetching it.
--
-- Wraps 'loadStackageSnapshot' without a shared HTTP manager (creates one
-- on demand).
loadStackageSnapshotWithMode :: String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshotWithMode = loadStackageSnapshot Nothing
