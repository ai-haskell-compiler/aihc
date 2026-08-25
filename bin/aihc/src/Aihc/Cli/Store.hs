-- | Filesystem layout shared by installed runtimes, libraries, and the
-- compiler that consumes them.
module Aihc.Cli.Store
  ( defaultStoreRoot,
    installedLibrariesActivePath,
    installedLibrariesRoot,
    installedRuntimeArchivePath,
  )
where

import Aihc.Native (NativeTarget, RuntimeGarbageCollector (..), nativeTargetStoreDirectory)
import System.Directory (XdgDirectory (XdgCache), getXdgDirectory)
import System.FilePath ((</>))

defaultStoreRoot :: IO FilePath
defaultStoreRoot = do
  cacheDirectory <- getXdgDirectory XdgCache "aihc"
  pure (cacheDirectory </> "store")

installedLibrariesRoot :: FilePath -> FilePath
installedLibrariesRoot storeRoot = storeRoot </> "libraries"

installedLibrariesActivePath :: FilePath -> FilePath
installedLibrariesActivePath storeRoot = installedLibrariesRoot storeRoot </> "active"

installedRuntimeArchivePath :: FilePath -> NativeTarget -> RuntimeGarbageCollector -> FilePath
installedRuntimeArchivePath storeRoot target garbageCollector =
  storeRoot
    </> "runtimes"
    </> nativeTargetStoreDirectory target
    </> renderGarbageCollector garbageCollector
    </> "runtime.a"

renderGarbageCollector :: RuntimeGarbageCollector -> FilePath
renderGarbageCollector garbageCollector =
  case garbageCollector of
    RuntimeGcCalloc -> "calloc"
    RuntimeGcSemispace -> "semispace"
