-- | This module re-exports most of the submodules.
module Data.RPM (
  module Data.RPM.NV,
  module Data.RPM.NVR,
  module Data.RPM.NVRA,
  module Data.RPM.VerRel,
  module Data.RPM.VerCmp,
  dropRelease,
  addRelease,
  dropArch,
  addArch
  ) where

import Data.RPM.NV
import Data.RPM.NVR
import Data.RPM.NVRA
import Data.RPM.VerCmp
import Data.RPM.VerRel

-- | Map a name-version-release into a name-version
dropRelease :: NVR -> NV
dropRelease (NVR n (VerRel v _)) = NV n v

-- | Add a release to name-version to make an NVR
addRelease :: NV -> String -> NVR
addRelease _ "" = error "addRelease: release cannot be empty"
addRelease (NV n v) r = NVR n (VerRel v r)

-- | Map a name-version-release.arch into a name-version-release
dropArch :: NVRA -> NVR
dropArch (NVRA n vr _) = NVR n vr

-- | Add an arch to NVR to make an NVRA
addArch :: NVR -> String -> NVRA
addArch _ "" = error "addArch: arch cannot be empty"
addArch (NVR n vr) a = NVRA n vr a
