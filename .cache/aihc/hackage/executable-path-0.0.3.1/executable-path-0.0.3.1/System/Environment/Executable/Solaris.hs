

{-
symbolic links to the executable:

Linux:
/proc/<pid>/exe

Solaris: (Solaris 10 only???)
/proc/<pid>/object/a.out (filename only)
/proc/<pid>/path/a.out (complete pathname)

*BSD:
/proc/<pid>/file
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module System.Environment.Executable.Solaris
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

import System.Posix
--import System.FilePath

--------------------------------------------------------------------------------

getPID :: IO Int
getPID = liftM fromIntegral $ getProcessID

getExecutablePath :: IO FilePath
getExecutablePath = do
  pid <- getPID
  fname <- readSymbolicLink $ "/proc/" ++ show pid ++ "/path/a.out"
  --let (path,exename) = splitFileName fname
  --return path
  return fname

--------------------------------------------------------------------------------
  
