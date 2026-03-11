{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.Config
-- Copyright   :  (c) Sven Panne 2003-2016
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This purely internal module defines the platform-specific stuff which has
-- been figured out by configure.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.Config (
   -- AL types
   ALboolean, ALchar, ALbyte, ALubyte, ALshort, ALushort, ALint, ALuint,
   ALsizei, ALenum, ALfloat, ALdouble,

   -- ALC types
   ALCboolean, ALCchar, ALCbyte, ALCubyte, ALCshort, ALCushort, ALCint, ALCuint,
   ALCsizei, ALCenum, ALCfloat, ALCdouble,

   -- Device stuff
   ALCdevice(ALCdevice), Device, nullDevice, marshalDevice, unmarshalDevice, closeDevice,

   -- Context stuff
   ALCcontext(..), Context, nullContext, marshalContext, unmarshalContext,
   alcProcessContext, alcMakeContextCurrent, alcDestroyContext
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.C.Types
import Foreign.Ptr ( Ptr, nullPtr )

--------------------------------------------------------------------------------
-- AL types

-- | 8-bit boolean
type ALboolean = CChar

-- | Character
type ALchar = CChar

-- | Signed 8-bit 2\'s complement integer
type ALbyte = CSChar

-- | Unsigned 8-bit integer
type ALubyte = CUChar

-- | Signed 16-bit 2\'s complement integer
type ALshort = CShort

-- | Unsigned 16-bit integer
type ALushort = CUShort

-- | Signed 32-bit 2\'s complement integer
type ALint = CInt

-- | Unsigned 32-bit integer
type ALuint = CUInt

-- | Non-negatitve 32-bit binary integer size
type ALsizei = CInt

-- | Enumerated 32-bit value
type ALenum = CInt

-- | 32-bit IEEE754 floating-point
type ALfloat = CFloat

-- | 64-bit IEEE754 floating-point
type ALdouble = CDouble

--------------------------------------------------------------------------------
-- ALC types

-- | 8-bit boolean
type ALCboolean = CChar

-- | Character
type ALCchar = CChar

-- | Signed 8-bit 2\'s complement integer
type ALCbyte = CSChar

-- | Unsigned 8-bit integer
type ALCubyte = CUChar

-- | Signed 16-bit 2\'s complement integer
type ALCshort = CShort

-- | Unsigned 16-bit integer
type ALCushort = CUShort

-- | Signed 32-bit 2\'s complement integer
type ALCint = CInt

-- | Unsigned 32-bit integer
type ALCuint = CUInt

-- | Non-negatitve 32-bit binary integer size
type ALCsizei = CInt

-- | Enumerated 32-bit value
type ALCenum = CInt

-- | 32-bit IEEE754 floating-point
type ALCfloat = CFloat

-- | 64-bit IEEE754 floating-point
type ALCdouble = CDouble

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcCloseDevice() returns an ALCboolean, before it was void.
-- To break a dependency cycle, we have to define the Device type here, too.

-- | The abstract device type.

newtype Device = Device ALCdevice
   deriving ( Eq, Ord, Show )

newtype ALCdevice = ALCdevice (Ptr ALCdevice)
   deriving ( Eq, Ord, Show )

nullDevice :: Device
nullDevice = Device (ALCdevice nullPtr)

marshalDevice :: Device -> ALCdevice
marshalDevice (Device device) = device

unmarshalDevice :: ALCdevice -> Maybe Device
unmarshalDevice device =
   if device == marshalDevice nullDevice then Nothing else Just (Device device)

-- | 'closeDevice' allows the application (i.e. the client program) to
-- disconnect from a device (i.e. the server). It returns 'True' for success and
-- 'False' for failure. Once closed, the 'Device' is invalid.
--
-- /Note:/ Older OpenAL implementations will always report a success!

closeDevice :: MonadIO m => Device -> m Bool
-- inlined unmarshalALCboolean here to break dependency cycle
closeDevice = liftIO . fmap (/= 0) . alcCloseDevice . marshalDevice

foreign import ccall unsafe "alcCloseDevice"
   alcCloseDevice :: ALCdevice -> IO ALCboolean

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcProcessContext() returns void for all platforms, before it
-- returned ALCcontext* on Linux. To break a dependency cycle, we have to define
-- the Context type here, too.

-- | The abstract context type.

data Context = Context ALCcontext
   deriving ( Eq, Ord, Show )

newtype ALCcontext = ALCcontext (Ptr ALCcontext)
   deriving ( Eq, Ord, Show )

nullContext :: Context
nullContext = Context (ALCcontext nullPtr)

marshalContext :: Context -> ALCcontext
marshalContext (Context context) = context

unmarshalContext :: ALCcontext -> Maybe Context
unmarshalContext context =
   if context == marshalContext nullContext then Nothing else Just (Context context)

--------------------------------------------------------------------------------
-- In ancient times, alcProcessContext returned ALCcontext.

foreign import ccall unsafe "alcProcessContext"
   alcProcessContext :: ALCcontext -> IO ()

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcMakeContextCurrent() returns void, before it was ALCenum on
-- Linux and ALCboolean on other platforms.

foreign import ccall unsafe "alcMakeContextCurrent"
   alcMakeContextCurrent :: ALCcontext -> IO ALCboolean

--------------------------------------------------------------------------------
-- In OpenAL 1.1, alcDestroyContext() returns void, before it returned ALCenum
-- on Linux.

foreign import ccall unsafe "alcDestroyContext"
   alcDestroyContext :: ALCcontext -> IO ()
