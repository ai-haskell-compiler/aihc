
-- It seems that on FreeBSD (and also other BSD systems), 
-- /proc is not mounted by default

{-
symbolic links to the executable:

Linux:
/proc/<pid>/exe

Solaris: (Solaris 10 only???)
/proc/<pid>/object/a.out (filename only)
/proc/<pid>/path/a.out (complete pathname)

*BSD:
/proc/<pid>/exe (NetBSD >= 4.0?)
/proc/<pid>/file (not a symbolic link?)
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module System.Environment.Executable.BSD
  ( getExecutablePath  
  , getPID
  )
  where

import Data.Bits
import Data.Word
import Data.Int

import Control.Monad

import Foreign
import Foreign.C

import System.Posix as Posix
import System.Directory
--import System.FilePath

--------------------------------------------------------------------------------

getPID :: IO Int
getPID = liftM fromIntegral $ getProcessID

getExecutablePath :: IO FilePath
getExecutablePath = do
  try1 <- getExecutablePathProcFS
  case try1 of
    Just path -> return path
    Nothing -> do
      try2 <- getExecutablePathUnderscoreFallback       
      case try2 of
        Just path -> return path
        Nothing -> error "getExecutablePath/BSD: unable to obtain the path"
      
-- Tries procfs. However, procfs is not always mounted on BSD systems... :(  
getExecutablePathProcFS :: IO (Maybe FilePath)
getExecutablePathProcFS = do
  -- since NetBSD 4.0, allegedly there is a symbolic link 
  -- "/proc/PID/exe", at least when procfs is mounted at all...
  try1 <- getExecutablePathProcFS' "exe"
  case try1 of
    Just _  -> return try1
    Nothing -> getExecutablePathProcFS' "file" 

-- eg. @getExecutablePathProcFS "exe"@
getExecutablePathProcFS' :: FilePath -> IO (Maybe FilePath)
getExecutablePathProcFS' symlink = do
  pid <- getPID
  let procPid  = "/proc/" ++ show pid ++ "/" ++ symlink
  Posix.fileExist procPid >>= \b -> if b 
    then Posix.getSymbolicLinkStatus procPid >>= \s -> if Posix.isSymbolicLink s
      then liftM Just $ Posix.readSymbolicLink procPid
      else return Nothing
    else return Nothing
  
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
  
