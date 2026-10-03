-- | The architecture name for 64-bit ARM, as GHC gives it.
module System.Info.Arch (arch) where

import Prelude (String)

-- | The machine architecture that the program runs on.
arch :: String
arch = "aarch64"
