{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.Extensions
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to the extension handling parts of section 6.3
-- (ALC Queries) of the OpenAL Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.Extensions (
   alcExtensions, alcIsExtensionPresent, alcProcAddress, alcEnumValue,
   alcVersion
) where

#if __GLASGOW_HASKELL__ >= 704
-- Make the foreign imports happy.
import Foreign.C.Types
#endif

import Control.Monad ( liftM2 )
import Data.StateVar ( GettableStateVar, makeGettableStateVar )
import Foreign.Ptr ( Ptr, FunPtr )

import Sound.OpenAL.ALC.BasicTypes
import Sound.OpenAL.ALC.Device
import Sound.OpenAL.ALC.QueryUtils
import Sound.OpenAL.ALC.String
import Sound.OpenAL.Config

--------------------------------------------------------------------------------

-- | Contains a list of available context extensions.

alcExtensions :: Device -> GettableStateVar [String]
alcExtensions device = makeGettableStateVar $
   fmap words $ getString (Just device) Extensions

--------------------------------------------------------------------------------

-- | The application is expected to verify the applicability of an extension or
-- core function entry point before requesting it by name, by use of
-- 'alcIsExtensionPresent'. Extension entry points can be retrieved using
-- 'alcProcAddress'. Entry points can be device specific, but are not context
-- specific. Using 'Nothing' as the device does not guarantee that the entry
-- point is returned, even if available for one of the available devices.

alcProcAddress :: Maybe Device -> String -> GettableStateVar (FunPtr a)
alcProcAddress maybeDevice funcName =
   makeGettableStateVar .
      withALCString funcName .
         alcGetProcAddress . marshalDevice . maybe nullDevice id $ maybeDevice

foreign import ccall unsafe "alcGetProcAddress"
   alcGetProcAddress :: ALCdevice -> Ptr ALCchar -> IO (FunPtr a)

--------------------------------------------------------------------------------

-- | Enumeration\/token values are device independent, but tokens defined for
-- extensions might not be present for a given device. Using 'Nothing' as the
-- device is legal, but only the tokens defined by the AL core are
-- guaranteed. Availability of extension tokens depends on the ALC extension.

alcEnumValue :: Maybe Device -> String -> GettableStateVar ALCenum
alcEnumValue maybeDevice enumName =
   makeGettableStateVar .
      withALCString enumName .
         alcGetEnumValue . marshalDevice . maybe nullDevice id $ maybeDevice

foreign import ccall unsafe "alcGetEnumValue"
   alcGetEnumValue :: ALCdevice -> Ptr ALCchar -> IO ALCenum

--------------------------------------------------------------------------------

-- | Contains the \"/major/./minor/\" specification revision for this implementation.

alcVersion :: GettableStateVar String
alcVersion =
   makeGettableStateVar $
      liftM2 makeVersionString
             (getInteger Nothing MajorVersion)
             (getInteger Nothing MinorVersion)

makeVersionString :: ALCint -> ALCint -> String
makeVersionString major minor = show major ++ "." ++ show minor
