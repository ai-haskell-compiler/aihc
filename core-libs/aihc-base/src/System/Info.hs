-- | Information about the platform that the program runs on.
--
-- Each value has one definition per platform. The cabal file selects the
-- definition with the source directory of the target operating system and
-- the source directory of the target architecture.
--
-- The compiler values give the GHC release that aihc emulates, as the
-- @__GLASGOW_HASKELL__@ macro does.
module System.Info
  ( os,
    arch,
    compilerName,
    compilerVersion,
    fullCompilerVersion,
  )
where

import Data.Version (Version (..))
import System.Info.Arch (arch)
import System.Info.OS (os)
import Prelude

-- | The name of the compiler that the program was compiled with.
compilerName :: String
compilerName = "ghc"

-- | The major and minor version of the compiler.
compilerVersion :: Version
compilerVersion = Version [9, 12] []

-- | The full version of the compiler.
fullCompilerVersion :: Version
fullCompilerVersion = Version [9, 12, 4] []
