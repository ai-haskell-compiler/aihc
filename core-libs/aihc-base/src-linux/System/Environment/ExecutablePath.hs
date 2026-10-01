{-# LANGUAGE CApiFFI #-}

-- | The path of the running executable on Linux. This is the implementation
-- of GHC: the target of the @/proc/self/exe@ symbolic link.
module System.Environment.ExecutablePath
  ( executablePath,
    getExecutablePath,
  )
where

import Data.List (isSuffixOf)
import Foreign.C.Error (throwErrnoPathIfMinus1)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Array (allocaArray0)
import System.Posix.Internals (peekFilePathLen, withFilePath)
import System.Posix.Types (CSsize (..))
import Prelude

foreign import capi unsafe "unistd.h readlink"
  c_readlink :: CString -> CString -> CSize -> IO CSsize

-- | The target of a symbolic link. Like the function of GHC, this reads at
-- most 4096 bytes of the target.
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
  allocaArray0 4096 $ \buffer ->
    withFilePath file $ \path -> do
      size <-
        throwErrnoPathIfMinus1 "readSymbolicLink" file
          $ c_readlink path buffer 4096
      peekFilePathLen (buffer, fromIntegral size)

-- | The absolute path of the running executable.
getExecutablePath :: IO FilePath
getExecutablePath = readSymbolicLink "/proc/self/exe"

-- | The absolute path of the running executable, or 'Nothing' when the
-- executable file no longer exists. procfs(5) appends @(deleted)@ to the
-- target of the link when the file was removed.
executablePath :: Maybe (IO (Maybe FilePath))
executablePath = Just (check <$> getExecutablePath)
  where
    check path
      | "(deleted)" `isSuffixOf` path = Nothing
      | otherwise = Just path
