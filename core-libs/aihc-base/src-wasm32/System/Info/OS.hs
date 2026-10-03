-- | The operating system name for WASI, as GHC gives it.
module System.Info.OS (os) where

import Prelude (String)

-- | The operating system that the program runs on.
os :: String
os = "wasi"
