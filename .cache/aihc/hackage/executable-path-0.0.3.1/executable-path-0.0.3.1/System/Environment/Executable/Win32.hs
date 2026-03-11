
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Environment.Executable.Win32
  ( getExecutablePath  
  , getModulePath
  )
  where

import Data.Bits
import Data.Word
import Data.Int

import Control.Monad

import Foreign
import Foreign.C
import Foreign.Marshal

--import System.Win32
--import System.Win32.DLL

--------------------------------------------------------------------------------

foreign import stdcall unsafe "Windows.h GetModuleFileNameW" c_GetModuleFileNameW
  :: HMODULE -> Ptr CWchar -> Word32 -> IO Word32

type HMODULE = Ptr ()

getModulePath :: HMODULE -> IO FilePath
getModulePath = getModulePath' 512

getModulePath' :: Word32 -> HMODULE -> IO FilePath
getModulePath' size hmodule = do
  mpath <- allocaArray0 (fromIntegral size) $ \p -> do
    k <- c_GetModuleFileNameW hmodule p size 
    case k of
      0 -> error "getModulePath: unknown error"
      _ -> if k == size
             then return Nothing
             else liftM Just $ peekCWString p
  case mpath of
    Just path -> return path
    Nothing -> getModulePath' (2*size) hmodule
             
{-
-- | Returns the full path + name of the module.
getModulePath :: HMODULE -> IO FilePath
getModulePath hmodule = getModuleFileName hmodule
-}

getExecutablePath :: IO FilePath  
getExecutablePath = getModulePath nullPtr
  
--------------------------------------------------------------------------------
