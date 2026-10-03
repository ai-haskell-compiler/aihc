-- | Information about the platform that the program runs on.
--
-- Each value has one definition per platform. The cabal file selects the
-- definition with the source directory of the target operating system and
-- the source directory of the target architecture.
module System.Info
  ( os,
    arch,
  )
where

import System.Info.Arch (arch)
import System.Info.OS (os)
