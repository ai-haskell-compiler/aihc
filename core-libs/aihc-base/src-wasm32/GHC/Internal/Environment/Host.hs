-- | The C environment of the process. WASI cannot start a child process, so
-- only the copy of the environment in the runtime is necessary. These
-- operations do nothing.
module GHC.Internal.Environment.Host
  ( hostSetEnv,
    hostUnsetEnv,
  )
where

import Prelude

hostSetEnv :: String -> String -> IO ()
hostSetEnv _ _ = return ()

hostUnsetEnv :: String -> IO ()
hostUnsetEnv _ = return ()
