{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Argv0
	( getArgv0
	) where

import           Prelude hiding (FilePath)

import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import           Foreign
import           Foreign.C
#if defined(mingw32_HOST_OS) && __GLASGOW_HASKELL__ >= 702
import           GHC.Environment (getFullArgs)
#endif

import           Filesystem.Path (FilePath)
import           Filesystem.Path.CurrentOS ()
import           Filesystem.Path.Rules (decode, posix, windows)

-- | Get @argv[0]@ as a 'FilePath'. This is how the program was invoked, and
-- might not correspond to any actual file.
--
-- Use this instead of @System.Environment.getProgName@ if you want the full
-- path, and not just the last component.
getArgv0 :: IO FilePath

#if defined(mingw32_HOST_OS) && __GLASGOW_HASKELL__ >= 702
getArgv0 = do
	m_argv0 <- getWin32ProgArgv0
	argv0 <- maybe (fmap head getFullArgs) return m_argv0
	return (decode windows (Text.pack argv0))

getWin32ProgArgv0 :: IO (Maybe String)
getWin32ProgArgv0 =
	alloca $ \p_argc ->
	alloca $ \p_argv -> do
		c_getWin32ProgArgv p_argc p_argv
		argv <- peek p_argv
		if argv == nullPtr
			then return Nothing
			else do
				argv0 <- peekCWString =<< peekElemOff argv 0
				return (Just argv0)

foreign import ccall unsafe "getWin32ProgArgv"
	c_getWin32ProgArgv :: Ptr CInt -> Ptr (Ptr CWString) -> IO ()

#else
getArgv0 =
	alloca $ \p_argc ->
	alloca $ \p_argv -> do
		c_getProgArgv p_argc p_argv
		argv <- peek p_argv
#ifdef mingw32_HOST_OS
		argv0 <- peekCString =<< peekElemOff argv 0
		return (decode windows (Text.pack argv0))
#else
		argv0 <- Bytes.packCString =<< peekElemOff argv 0
		return (decode posix argv0)
#endif

foreign import ccall unsafe "getProgArgv"
	c_getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

#endif
