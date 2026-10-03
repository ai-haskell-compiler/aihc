-- | The architecture name for 64-bit x86, as GHC gives it.
module System.Info.Arch (arch) where

import Prelude (String)

-- | The machine architecture that the program runs on.
arch :: String
arch = "x86_64"
