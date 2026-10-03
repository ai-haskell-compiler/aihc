-- | The architecture name for wasm32, as GHC gives it.
module System.Info.Arch (arch) where

import Prelude (String)

-- | The machine architecture that the program runs on.
arch :: String
arch = "wasm32"
