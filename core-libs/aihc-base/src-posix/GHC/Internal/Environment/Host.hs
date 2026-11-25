{-# LANGUAGE CApiFFI #-}

-- | The C environment of the process. A child process that the process
-- package starts gets this environment, so 'System.Environment.setEnv' must
-- change it too, as in GHC.
module GHC.Internal.Environment.Host
  ( hostSetEnv,
    hostUnsetEnv,
  )
where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Prelude

-- | Give a value to a variable of the C environment.
hostSetEnv :: String -> String -> IO ()
hostSetEnv name value =
  withCString name (\cName -> withCString value (\cValue -> c_setenv cName cValue 1 >> return ()))

-- | Remove a variable from the C environment.
hostUnsetEnv :: String -> IO ()
hostUnsetEnv name = withCString name (\cName -> c_unsetenv cName >> return ())

foreign import capi unsafe "stdlib.h setenv"
  c_setenv :: CString -> CString -> CInt -> IO CInt

foreign import capi unsafe "stdlib.h unsetenv"
  c_unsetenv :: CString -> IO CInt
