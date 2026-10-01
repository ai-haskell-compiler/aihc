-- | The path of the running executable on WebAssembly. This is the fallback
-- of GHC for a platform without a way to ask for the path: the first
-- program argument that the host gave, before any change from 'withArgs' or
-- 'withProgName'.
module System.Environment.ExecutablePath
  ( executablePath,
    getExecutablePath,
  )
where

import Foreign.C.String (CString)
import Foreign.Ptr (nullPtr)
import System.Posix.Internals (peekFilePath)
import Prelude

foreign import ccall unsafe "aihc_program_initial_name"
  c_initialProgramName :: IO CString

-- | The first program argument that the host gave.
getExecutablePath :: IO FilePath
getExecutablePath = do
  name <- c_initialProgramName
  if name /= nullPtr
    then peekFilePath name
    else errorWithoutStackTrace ("getExecutablePath: " ++ message)
  where
    message =
      "no OS specific implementation and program name couldn't be "
        ++ "found in argv"

-- | No way to ask for the path exists on this platform.
executablePath :: Maybe (IO (Maybe FilePath))
executablePath = Nothing
