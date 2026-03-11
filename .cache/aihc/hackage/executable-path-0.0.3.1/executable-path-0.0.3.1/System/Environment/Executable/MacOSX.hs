
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module System.Environment.Executable.MacOSX
  ( getExecutablePath 
  , getApplicationBundlePath
  )
  where

import Data.Bits
import Data.Word
import Data.Int

import Control.Monad

import Foreign
import Foreign.C
 
--import System.FilePath
  
--------------------------------------------------------------------------------

type UInt8    = Word8
type UInt16   = Word16
type UInt32   = Word32
type UInt64   = Word64

type SInt8    = Int8
type SInt16   = Int16
type SInt32   = Int32
type SInt64   = Int64

type OSErr    = SInt16
type OSStatus = SInt32

type Boolean  = Bool
type Float32  = Float
type Float64  = Double

type UniChar   = Char
type CFIndex   = SInt32
type ItemCount = UInt32
type ByteCount = UInt32

data CFData
data CFString
data CFAllocator

type CFDataRef      = Ptr CFData    
type CFStringRef    = Ptr CFString
type CFAllocatorRef = Ptr CFAllocator

--------------------------------------------------------------------------------

kCFAllocatorDefault :: CFAllocatorRef
kCFAllocatorDefault = nullPtr

osStatusString :: OSStatus -> String
osStatusString osstatus = "OSStatus = " ++ show osstatus

osStatusError :: OSStatus -> IO a
osStatusError osstatus = fail $ osStatusString osstatus

foreign import ccall unsafe "CFBase.h CFRelease" 
  c_CFRelease :: Ptr a -> IO ()

foreign import ccall unsafe "CFString.h CFStringGetLength" 
  c_CFStringGetLength :: CFStringRef -> IO CFIndex

foreign import ccall unsafe "CFString.h CFStringGetCharactersPtr"
  c_CFStringGetCharactersPtr :: CFStringRef -> IO (Ptr UniChar)  

foreign import ccall unsafe "CFString.h CFStringGetCharacterAtIndex"
  c_CFStringGetCharacterAtIndex :: CFStringRef -> CFIndex -> IO UniChar 

foreign import ccall unsafe "CFString.h CFStringCreateWithCharacters"
  c_CFStringCreateWithCharacters :: CFAllocatorRef -> Ptr UniChar -> CFIndex -> IO CFStringRef

-- | Manually releasing a CFString.
releaseCFString :: CFStringRef -> IO ()
releaseCFString = c_CFRelease

-- | Peeks a CFString.
peekCFString :: CFStringRef -> IO String
peekCFString cfstring = do
  n <- c_CFStringGetLength cfstring
  p <- c_CFStringGetCharactersPtr cfstring
  if p /= nullPtr 
    then forM [0..n-1] $ \i -> peekElemOff p (fromIntegral i)
    else forM [0..n-1] $ \i -> c_CFStringGetCharacterAtIndex cfstring i
 
-- | Creates a new CFString. You have to release it manually.
newCFString :: String -> IO CFStringRef
newCFString string = 
  let n = length string in allocaArray n $ \p ->
  c_CFStringCreateWithCharacters kCFAllocatorDefault p (fromIntegral n)
 
-- | Safe passing of a CFString to the OS (releases it afterwards).
withCFString :: String -> (CFStringRef -> IO a) -> IO a
withCFString string action = do
  cfstring <- newCFString string
  x <- action cfstring
  releaseCFString cfstring
  return x

-------------------------------------------------------------------------------- 
  
data CFBundle
type CFBundleRef = Ptr CFBundle

data CFURL
type CFURLRef = Ptr CFURL

type OSXEnum = CInt -- ?????????????
type CFURLPathStyle = OSXEnum

foreign import ccall unsafe "CFBundle.h CFBundleGetMainBundle" 
  c_CFBundleGetMainBundle :: IO CFBundleRef

foreign import ccall unsafe "CFBundle.h CFBundleCopyBundleURL" 
  c_CFBundleCopyBundleURL :: CFBundleRef -> IO CFURLRef

foreign import ccall unsafe "CFBundle.h CFBundleCopyExecutableURL"
  c_CFBundleCopyExecutableURL :: CFBundleRef -> IO CFURLRef

foreign import ccall unsafe "CFURL.h CFURLCopyFileSystemPath" 
  c_CFURLCopyFileSystemPath :: CFURLRef -> CFURLPathStyle -> IO CFStringRef

kCFURLPOSIXPathStyle   = 0 :: CFURLPathStyle
kCFURLHFSPathStyle     = 1 :: CFURLPathStyle
kCFURLWindowsPathStyle = 2 :: CFURLPathStyle

-- | Mac OS X only.
getApplicationBundlePath :: IO FilePath
getApplicationBundlePath = do
  bundle <- c_CFBundleGetMainBundle
  url    <- c_CFBundleCopyBundleURL bundle
  cfpath <- c_CFURLCopyFileSystemPath url kCFURLPOSIXPathStyle
  peekCFString cfpath

getExecutablePath :: IO FilePath
getExecutablePath = do 
  bundle <- c_CFBundleGetMainBundle
  url    <- c_CFBundleCopyExecutableURL bundle
  cfpath <- c_CFURLCopyFileSystemPath url kCFURLPOSIXPathStyle
  fname <- peekCFString cfpath
--  let (path,exename) = splitFileName fname
--  return path
  return fname


--------------------------------------------------------------------------------
