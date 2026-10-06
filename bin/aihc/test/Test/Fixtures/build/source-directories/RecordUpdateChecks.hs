module RecordUpdateChecks (recordUpdateChecks) where

import HiddenRecord
import RecordFacade

-- A record update names fields and rebuilds a constructor that the export
-- list of a module hides: once in the module that defines it, and once in
-- a module that re-exports the type without it.
hiddenByDefiner :: Options
hiddenByDefiner = defaultOptions {verbose = False}

hiddenByFacade :: Settings
hiddenByFacade = defaultSettings {width = 120}

recordUpdateChecks :: Bool
recordUpdateChecks =
  limit hiddenByDefiner == 1
    && not (verbose hiddenByDefiner)
    && width hiddenByFacade == 120
    && wrap hiddenByFacade
