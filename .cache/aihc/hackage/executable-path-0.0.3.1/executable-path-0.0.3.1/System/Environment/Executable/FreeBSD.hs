
-- | This code uses @sysctl@ and @KERN_PROC_PATHNAME@,
-- if we are on FreeBSD 6.0 or newer, and falls back to procfs
-- on older FreeBSD-s.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.Environment.Executable.FreeBSD
  ( getExecutablePath  
  , getPID
  )
  where
  
import Data.Bits
import Data.Word
import Data.Int

import Data.Char
import Data.List

import Control.Monad

import Foreign
import Foreign.C

import System.Posix as Posix
import System.Directory

--------------------------------------------------------------------------------

#define CTL_KERN             1 
#define KERN_PROC           14    
#define KERN_PROC_PATHNAME  12   
-- KERN_PROC_PATHNAME exists from FreeBSD 6.0

#define ENOMEM    12

#define KERN_OSTYPE    1
#define KERN_OSRELEASE 2
#define KERN_OSREV     3
-- KERN_OSREV gives back a totally random-looking
-- number with a totally undocumented meaning, yeah, fuck that.

#define KERN_VERSION   4

--------------------------------------------------------------------------------

-- the only data point is the string "6.4-RELEASE"
-- yay for undocumented queries!
-- not let's try to parse that
parseOSRelease :: String -> Maybe (Int,Int)
parseOSRelease text = 
  if major /= "" && temp1 /= "" && minor /= "" && dot == '.'
    then Just (read major, read minor)
    else Nothing
  where
    (major,temp1) = span isDigit text
    (dot:temp2)   = temp1
    (minor,rest)  = span isDigit temp2

getExecutablePath :: IO FilePath
getExecutablePath = do
{-
  osrev <- getKernInt KERN_OSREV
  ostype <- getKernString KERN_OSTYPE 256
  kver  <- getKernString KERN_VERSION 256
  print osrev
  print ostype
  print kver
-}

  osrel  <- getKernString KERN_OSRELEASE 256
--  print osrel
--  print $ parseOSRelease osrel
  case parseOSRelease osrel of
    Just (maj,_) -> if maj >= 6
      then getExecutablePathSysCtl 256
      else getExecutablePathProcFS
    Nothing -> getExecutablePathProcFS

--------------------------------------------------------------------------------

-- int sysctl(int *name, u_int namelen, void *oldp, size_t *oldlenp, void *newp, size_t newlen);
foreign import ccall "sys/sysctl.h sysctl" 
  sysctl :: Ptr CInt -> CUInt -> Ptr a -> Ptr CSize -> Ptr a -> CSize -> IO CInt

-- brrrrrrr...
foreign import ccall "errno.h &errno" errno :: Ptr CInt
      
getKernString :: CInt -> Int -> IO String
getKernString what len = do
  allocaArray 2 $ \mib -> do
    pokeArray mib [ CTL_KERN, what ]  
    alloca $ \buflen -> do
      poke buflen (fromIntegral len :: CSize)
      allocaBytes len $ \buf -> do
        sysctl mib 2 buf buflen nullPtr 0
        peekCString buf

getKernInt :: CInt -> IO CInt
getKernInt what = do
  allocaArray 2 $ \mib -> do
    pokeArray mib [ CTL_KERN, what ]  
    alloca $ \posrev -> do
      alloca $ \buflen -> do
        poke buflen (fromIntegral (sizeOf (undefined :: CInt)) :: CSize)
        sysctl mib 2 posrev buflen nullPtr 0
        peek posrev
      
getExecutablePathSysCtl :: Int -> IO FilePath
getExecutablePathSysCtl size = do
  allocaArray 4 $ \mib -> do
    pokeArray mib [ CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1 ]   -- @-1@ is current process (could be a PID)
    allocaBytes size $ \buf -> do
      alloca $ \buflen -> do
        poke buflen (fromIntegral size :: CSize)
        err <- sysctl mib 4 buf buflen nullPtr 0
        case err of
          0 -> peekCString buf
          -1 -> do
            errn <- peek errno
            case errn of
              ENOMEM -> getExecutablePathSysCtl (size*2)
              _ -> error "getExecutablePath: unknown system error"          

{-
int mib[4];
mib[0] = CTL_KERN;
mib[1] = KERN_PROC;
mib[2] = KERN_PROC_PATHNAME;
mib[3] = -1;
char buf[1024];
size_t cb = sizeof(buf);
sysctl(mib, 4, buf, &cb, NULL, 0);
-}

--------------------------------------------------------------------------------

-- procfs fallback for FreeBSD < 6.0

getPID :: IO Int
getPID = liftM fromIntegral $ getProcessID

{-
getExecutablePathProcFS :: IO FilePath
getExecutablePathProcFS = do
  pid <- getPID
  let procPid = "/proc/" ++ show pid ++ "/file"
  fname <- readSymbolicLink procPid
  return fname
-}

getExecutablePathProcFS :: IO FilePath
getExecutablePathProcFS = do
  try1 <- getExecutablePathProcFS' "file"
  case try1 of
    Just xx -> return xx
    Nothing -> do
      try2 <- getExecutablePathUnderscoreFallback
      case try2 of
        Just yy -> return yy
        Nothing -> error "getExecutablePath/FreeBSD: unable to obtain the path"
      
-- eg. @getExecutablePathProcFS "file"@
getExecutablePathProcFS' :: FilePath -> IO (Maybe FilePath)
getExecutablePathProcFS' symlink = do
  pid <- getPID
  let procPid  = "/proc/" ++ show pid ++ "/" ++ symlink
  Posix.fileExist procPid >>= \b -> if b 
    then Posix.getSymbolicLinkStatus procPid >>= \s -> if Posix.isSymbolicLink s
      then liftM Just $ Posix.readSymbolicLink procPid
      else return Nothing
    else return Nothing

--------------------------------------------------------------------------------

-- even more fallback, if for some reason procfs doesn't work
      
-- this is an unreliable fallback trying to 
-- get the environment variable named "_".  
getExecutablePathUnderscoreFallback :: IO (Maybe FilePath) 
getExecutablePathUnderscoreFallback = do
  mp <- getEnv "_" 
  case mp of
    Nothing -> return mp
    Just p -> do
      q <- canonicalizePath p
      return (Just q)  

--------------------------------------------------------------------------------


