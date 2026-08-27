-- | GHC session initialization for reading .hi interface files.
--
-- Provides a minimal GHC session sufficient to deserialize .hi files
-- using 'GHC.Iface.Binary.readBinIface'. This avoids the overhead of
-- a full GHC compilation session while still providing the 'NameCache'
-- and 'Profile' required for deserialization.
module Aihc.Dev.ExtractHi.GhcSession
  ( withReadIface,
  )
where

import GHC (Ghc, getSession, runGhc, setSessionDynFlags)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Monad (getSessionDynFlags)
import GHC.Driver.Session (DynFlags (..))
import GHC.Iface.Binary (CheckHiWay (..), TraceBinIFace (..), readBinIface)
import GHC.Platform.Profile (Profile (..))
import GHC.Platform.Ways qualified as Ways
import GHC.Unit.Module.ModIface (ModIface)
import System.Process (readProcess)

-- | Run an action with a function that reads .hi files.
--
-- Initializes a GHC session to obtain the 'NameCache' and host 'Profile',
-- then provides a reader function to the callback. The reader can be called
-- repeatedly to deserialize multiple .hi files within the same session.
withReadIface :: (ReadIface -> Ghc a) -> IO a
withReadIface action = do
  libDir <- getGhcLibDir
  runGhc (Just libDir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    env <- getSession
    let profile =
          Profile
            { profilePlatform = targetPlatform (hsc_dflags env),
              profileWays = Ways.hostFullWays
            }
        nc = hsc_NC env
        reader = readBinIface profile nc IgnoreHiWay QuietBinIFace
    action reader

-- | A function that reads a .hi file into a 'ModIface'.
type ReadIface = FilePath -> IO ModIface

-- | Discover the GHC library directory by calling @ghc --print-libdir@.
getGhcLibDir :: IO FilePath
getGhcLibDir = do
  raw <- readProcess "ghc" ["--print-libdir"] ""
  pure (trimTrailingNewline raw)
  where
    trimTrailingNewline = reverse . dropWhile (== '\n') . reverse
