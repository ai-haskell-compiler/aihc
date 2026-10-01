{-# LANGUAGE CApiFFI #-}

-- | The path of the running executable on Apple platforms. This is the
-- implementation of GHC: the path from @_NSGetExecutablePath@, which
-- @realpath@ then makes absolute and free of symbolic links.
module System.Environment.ExecutablePath
  ( executablePath,
    getExecutablePath,
  )
where

import Control.Exception.Base (catch, throw)
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Internals (peekFilePath, withFilePath)
import Prelude

foreign import capi unsafe "mach-o/dyld.h _NSGetExecutablePath"
  c_NSGetExecutablePath :: CString -> Ptr Word32 -> IO CInt

foreign import capi unsafe "stdlib.h realpath"
  c_realpath :: CString -> CString -> IO CString

-- | The path that the loader used to start the executable. It can contain
-- symbolic links and @..@ components.
nsGetExecutablePath :: IO FilePath
nsGetExecutablePath =
  -- PATH_MAX is 1024 on Apple platforms.
  allocaBytes 1024 $ \buffer ->
    alloca $ \bufferSize -> do
      poke bufferSize 1024
      status <- c_NSGetExecutablePath buffer bufferSize
      if status == 0
        then peekFilePath buffer
        else do
          requiredSize <- fromIntegral <$> peek bufferSize
          allocaBytes requiredSize $ \largerBuffer -> do
            status2 <- c_NSGetExecutablePath largerBuffer bufferSize
            if status2 == 0
              then peekFilePath largerBuffer
              else errorWithoutStackTrace "_NSGetExecutablePath: buffer too small"

realpath :: FilePath -> IO FilePath
realpath path =
  withFilePath path $ \fileName ->
    allocaBytes 1024 $ \resolvedName -> do
      _ <- throwErrnoIfNull "realpath" (c_realpath fileName resolvedName)
      peekFilePath resolvedName

-- | The absolute path of the running executable.
getExecutablePath :: IO FilePath
getExecutablePath = nsGetExecutablePath >>= realpath

-- | The absolute path of the running executable, or 'Nothing' when the
-- executable file no longer exists.
executablePath :: Maybe (IO (Maybe FilePath))
executablePath = Just (fmap Just getExecutablePath `catch` handler)
  where
    handler exception
      | isDoesNotExistError exception = pure Nothing
      | otherwise = throw exception
