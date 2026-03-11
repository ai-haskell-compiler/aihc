module GHC.Platform.Host where

import GHC.Platform.ArchOS

hostPlatformArch :: Arch
hostPlatformArch = ArchX86_64

hostPlatformOS   :: OS
hostPlatformOS   = OSDarwin

hostPlatformArchOS :: ArchOS
hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS
