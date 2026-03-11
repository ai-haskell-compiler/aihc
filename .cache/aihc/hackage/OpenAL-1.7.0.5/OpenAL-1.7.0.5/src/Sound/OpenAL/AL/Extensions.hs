{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.Extensions
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.1.2 (String Queries) and chapter 7
-- (Appendix: Extensions) of the OpenAL Specification and Reference (version
-- 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.Extensions (
   -- * General AL extension support
   alExtensions, alIsExtensionPresent, alProcAddress, alEnumValue, alVersion,

   -- * Internal re-exports, use at your own risk
   unmarshalALboolean, unmarshalFormat, unmarshalBuffer
) where

#if __GLASGOW_HASKELL__ >= 704
-- Make the foreign imports happy.
import Foreign.C.Types
#endif

import Data.StateVar ( GettableStateVar, makeGettableStateVar )
import Foreign.Ptr

import Sound.OpenAL.AL.ALboolean
import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.AL.BufferInternal
import Sound.OpenAL.AL.Format
import Sound.OpenAL.AL.QueryUtils
import Sound.OpenAL.AL.String

--------------------------------------------------------------------------------

-- | Contains the list of available extensions.

alExtensions :: GettableStateVar [String]
alExtensions = makeGettableStateVar (fmap words $ getString Extensions)

--------------------------------------------------------------------------------

-- | Contains a version string in the format @\"/\<spec major number\>/./\<spec
-- minor number\>/ /\<optional vendor version information\>/\"@.

alVersion :: GettableStateVar String
alVersion = makeGettableStateVar (getString Version)

--------------------------------------------------------------------------------

-- | To verify that a given extension is available for the current context, use
-- 'alIsExtensionPresent'. For invalid and unsupported string tokens it contains
-- 'False'. The extension name is not case sensitive: The implementation will
-- convert the name to all upper-case internally (and will express extension
-- names in upper-case).

alIsExtensionPresent :: String -> GettableStateVar Bool
alIsExtensionPresent extensionName =
   makeGettableStateVar $
      fmap unmarshalALboolean $
         withALString extensionName alIsExtensionPresent_

foreign import ccall unsafe "alIsExtensionPresent"
   alIsExtensionPresent_ :: Ptr ALchar -> IO ALboolean

--------------------------------------------------------------------------------

-- | To retrieving function entry addresses, applications can use
-- 'alProcAddress'. It contains 'nullFunPtr' if no entry point with the given
-- name can be found. Implementations are free to use 'nullFunPtr' if an
-- entry point is present, but not applicable for the current context. However
-- the specification does not guarantee this behavior. Applications can use
-- 'alProcAddress' to obtain core API entry points, not just extensions. This
-- is the recommended way to dynamically load and unload OpenAL DLLs as sound
-- drivers.

alProcAddress :: String -> GettableStateVar (FunPtr a)
alProcAddress funcName = makeGettableStateVar $
   withALString funcName alGetProcAddress

foreign import ccall unsafe "alGetProcAddress"
   alGetProcAddress :: Ptr ALchar -> IO (FunPtr a)

--------------------------------------------------------------------------------

-- | To obtain enumeration values for extensions, the application has to use
-- 'alEnumValue' of an extension token. Enumeration values are defined within
-- the OpenAL name space and allocated according to specification of the core
-- API and the extensions, thus they are context-independent.
--
-- 'alEnumValue' contains 0 if the enumeration can not be found. The presence of
-- an enum value does not guarantee the applicability of an extension to the
-- current context. A non-zero value indicates merely that the implementation is
-- aware of the existence of this extension. Implementations should not attempt
-- to use 0 to indicate that the extensions is not supported for the current
-- context.

alEnumValue :: String -> GettableStateVar ALenum
alEnumValue enumName = makeGettableStateVar $
   withALString enumName alGetEnumValue

foreign import ccall unsafe "alGetEnumValue"
   alGetEnumValue :: Ptr ALchar -> IO ALenum
