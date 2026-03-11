{-# LANGUAGE ForeignFunctionInterface #-}

module System.Environment.FindBin
    ( __Bin__
    , getProgPath
    ) where

import Foreign (Ptr, alloca, peek, peekElemOff)
import Foreign.C (CInt, CString, peekCString)
import System.Directory (canonicalizePath, findExecutable)
import System.FilePath (takeDirectory, takeBaseName)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE __Bin__ #-}

-- | Unsafe (/constant/) version of 'getProgPath'.
__Bin__ :: String
__Bin__ = let path = unsafePerformIO getProgPath
    in length path `seq` path

-- | Get the full directory to the running program.
getProgPath :: IO String
getProgPath = alloca $ \p_argc -> alloca $ \p_argv -> do
    getProgArgv p_argc p_argv
    argv <- peek p_argv
    arg0 <- peekCString =<< peekElemOff argv 0
    case arg0 of
        "<interactive>" -> alloca $ \p_argc' -> alloca $ \p_argv' -> do
            getFullProgArgv p_argc' p_argv'
            argc' <- peek p_argc'
            argv' <- peek p_argv'
            findBin =<< peekCString =<< peekElemOff argv' (pred $ fromEnum argc')
        _ -> do
            findBin arg0
    where
    directoryOf "" = directoryOf "."
    directoryOf x = do
        x' <- canonicalizePath x
        let path = takeDirectory x'
        return (length path `seq` path)
    findBin s = case takeDirectory s of
        ""  -> do
            -- This should work for ghci as well, as long as nobody name
            -- their executable file "<interactive>"...
            rv <- findExecutable s
            case rv of
                Just fullName   -> directoryOf fullName
                _               -> alloca $ \p_argc' -> alloca $ \p_argv' -> do
                    -- Here we are in the "runghc"/"runhaskell" land.  Fun!
                    getFullProgArgv p_argc' p_argv'
                    argc'   <- peek p_argc'
                    argv'   <- peek p_argv'
                    prog    <- peekCString =<< peekElemOff argv' 0
                    s'      <- case takeBaseName prog of
                        "runghc" -> peekCString =<< peekElemOff argv' (fromEnum argc'-1)
                        "runhaskell" -> peekCString =<< peekElemOff argv' (fromEnum argc'-1)
                        _ -> return prog
                    canon   <- canonicalizePath s
                    canon'  <- canonicalizePath s'
                    if canon == canon'
                        then findBin canon
                        else findBin s'
        _   -> directoryOf s

foreign import ccall unsafe "getFullProgArgv"
  getFullProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
