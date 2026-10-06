{-# LANGUAGE ForeignFunctionInterface #-}

-- The file functions of the libc, through the FFI. On wasm32-wasip3 the libc
-- calls the host through WASI 0.3 interfaces, so a call that needs the host
-- has to work here as it does on the other targets. The program works in one
-- directory that it makes and removes.
module Main where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import System.IO

foreign import ccall unsafe "mkdir" c_mkdir :: CString -> CUInt -> IO CInt

foreign import ccall unsafe "rmdir" c_rmdir :: CString -> IO CInt

foreign import ccall unsafe "unlink" c_unlink :: CString -> IO CInt

foreign import ccall unsafe "rename" c_rename :: CString -> CString -> IO CInt

-- WASI has no file modes, so the runtime accepts the call and leaves the file
-- as it is. A native host changes the mode and also returns zero.
foreign import ccall unsafe "chmod" c_chmod :: CString -> CUInt -> IO CInt

foreign import ccall unsafe "access" c_access :: CString -> CInt -> IO CInt

foreign import ccall unsafe "opendir" c_opendir :: CString -> IO (Ptr ())

foreign import ccall unsafe "readdir" c_readdir :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "closedir" c_closedir :: Ptr () -> IO CInt

work :: String
work = "aihc-libc-files"

-- Whether a path exists: access with F_OK, which is zero on every target.
exists :: String -> IO Bool
exists path = withCString path (\c -> (== 0) <$> c_access c 0)

-- The number of entries of a directory, which includes . and ..
entries :: String -> IO Int
entries path = withCString path $ \c -> do
  directory <- c_opendir c
  if directory == nullPtr
    then pure (-1)
    else do
      let count total = do
            entry <- c_readdir directory
            if entry == nullPtr then pure total else count (total + 1)
      total <- count (0 :: Int)
      _ <- c_closedir directory
      pure total

main :: IO ()
main = do
  made <- withCString work (\c -> c_mkdir c 0o755)
  putStrLn ("mkdir: " ++ show made)
  writeFile (work ++ "/one.txt") "one\n"
  writeFile (work ++ "/two.txt") "two\n"
  mode <- withCString (work ++ "/one.txt") (\path -> c_chmod path 0o644)
  putStrLn ("chmod: " ++ show mode)
  count <- entries work
  putStrLn ("entries: " ++ show count)
  before <- exists (work ++ "/one.txt")
  absent <- exists (work ++ "/absent.txt")
  putStrLn ("one.txt exists: " ++ show before ++ ", absent.txt exists: " ++ show absent)
  moved <- withCString (work ++ "/one.txt") (\from -> withCString (work ++ "/uno.txt") (c_rename from))
  after <- exists (work ++ "/uno.txt")
  stale <- exists (work ++ "/one.txt")
  putStrLn ("rename: " ++ show moved ++ ", uno.txt exists: " ++ show after ++ ", one.txt exists: " ++ show stale)
  mapM_ (\name -> withCString (work ++ "/" ++ name) c_unlink) ["uno.txt", "two.txt"]
  removed <- withCString work c_rmdir
  gone <- exists work
  putStrLn ("rmdir: " ++ show removed ++ ", work exists: " ++ show gone)
  hFlush stdout
