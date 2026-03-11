
{- |

The documentation of "System.Environment.getProgName" says that

\"However, this is hard-to-impossible to implement on some non-Unix OSes, 
so instead, for maximum portability, we just return the leafname 
of the program as invoked. Even then there are some differences 
between platforms: on Windows, for example, a program invoked as 
foo is probably really FOO.EXE, and that is what "getProgName" will 
return.\"

This library tries to fix this issue.
It also provides some platform-specific functions (most notably getting
the path of the application bundle on OSX). Supported operating
systems:
 
 * Win32 (tested on Windows 7)
 
 * Mac OS X
 
 * Linux

 * FreeBSD (tested on FreeBSD 6.4)

 * \*BSD (with procfs mounted, plus fallback for certain shells; untested)
 
 * Solaris (untested, and probably works on Solaris 10 only) 
 
-}

{-# LANGUAGE CPP #-}

module System.Environment.Executable
  ( getExecutablePath 
  , splitExecutablePath

#ifdef mingw32_HOST_OS 
  , getModulePath
#endif
 
#ifdef darwin_HOST_OS 
  , getApplicationBundlePath
#endif

#ifdef WE_HAVE_GHC
  , ScriptPath(..)
  , getScriptPath
#endif
  
  )
  where

import Control.Monad (liftM)
import System.FilePath (splitFileName)
import System.Directory (canonicalizePath)
import Data.Char (toLower)
import Data.List (find,findIndex)

#ifdef WE_HAVE_GHC
import GHC.Environment
#endif

--------------------------------------------------------------------------------

#ifdef mingw32_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.Win32
#endif

#ifdef darwin_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.MacOSX
#endif

#ifdef linux_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.Linux
#endif

#ifdef freebsd_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.FreeBSD
#endif

#ifdef netbsd_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.BSD
#endif

#ifdef openbsd_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.BSD
#endif

#ifdef solaris_HOST_OS
#define SUPPORTED_OS
import System.Environment.Executable.Solaris
#endif

--------------------------------------------------------------------------------

splitExecutablePath :: IO (FilePath,FilePath)
splitExecutablePath = liftM splitFileName getExecutablePath

--------------------------------------------------------------------------------

#ifndef SUPPORTED_OS
{-# WARNING getExecutablePath "the host OS is not supported!" #-}
getExecutablePath :: IO String
getExecutablePath = error "host OS not supported"
#endif

--------------------------------------------------------------------------------

#ifdef WE_HAVE_GHC

-- | An experimental hack which tries to figure out if the program
-- was run with @runghc@ or @runhaskell@ or @ghci@, and then tries to find 
-- out the directory of the /source/ (or object file).
--
-- GHC only.
getScriptPath :: IO ScriptPath
getScriptPath = do
  fargs <- getFullArgs
  exec  <- getExecutablePath
  let (pt,fn) = splitFileName exec
  case fargs of
    [] -> return (Executable exec)
    _  -> case map toLower fn of
#ifdef mingw32_HOST_OS
      "ghc.exe" -> do
#else
      "ghc" -> do
#endif
        case find f1 fargs of       
          Just s  -> do
            path <- canonicalizePath $ init (drop n1 s)
            return $ RunGHC path 
          Nothing -> case findIndex f2 fargs of
            Just i  -> return Interactive
            Nothing -> return (Executable exec)
      _ -> return (Executable exec)

  where
    f1 xs = take n1 xs == s1
    s1 = ":set prog \""
    n1 = length s1

    f2 xs = xs == "--interactive"
            
data ScriptPath
  = Executable FilePath  -- ^ it was (probably) a proper compiled executable
  | RunGHC FilePath      -- ^ it was a script run by runghc/runhaskell
  | Interactive          -- ^ we are in GHCi
  deriving Show
  
#endif

--------------------------------------------------------------------------------
