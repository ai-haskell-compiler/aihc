-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Instruments (
  -- * Extension Support
    gl_SGIX_instruments

  -- * GL_SGIX_instruments
  , glGetInstrumentsSGIX
  , glInstrumentsBufferSGIX
  , glPollInstrumentsSGIX
  , glReadInstrumentsSGIX
  , glStartInstrumentsSGIX
  , glStopInstrumentsSGIX
  , pattern GL_INSTRUMENT_BUFFER_POINTER_SGIX
  , pattern GL_INSTRUMENT_MEASUREMENTS_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/instruments.txt GL_SGIX_instruments> extension is available.

gl_SGIX_instruments :: Bool
gl_SGIX_instruments = member "GL_SGIX_instruments" extensions
{-# NOINLINE gl_SGIX_instruments #-}

-- | Usage: @'glGetInstrumentsSGIX'@


glGetInstrumentsSGIX :: MonadIO m => m GLint
glGetInstrumentsSGIX = ffiIOint glGetInstrumentsSGIXFunPtr

glGetInstrumentsSGIXFunPtr :: FunPtr (IO GLint)
glGetInstrumentsSGIXFunPtr = unsafePerformIO (getProcAddress "glGetInstrumentsSGIX")

{-# NOINLINE glGetInstrumentsSGIXFunPtr #-}

-- | Usage: @'glInstrumentsBufferSGIX' size buffer@
--
-- The length of @buffer@ should be @size@.


glInstrumentsBufferSGIX :: MonadIO m => GLsizei -> Ptr GLint -> m ()
glInstrumentsBufferSGIX = ffisizeiPtrintIOV glInstrumentsBufferSGIXFunPtr

glInstrumentsBufferSGIXFunPtr :: FunPtr (GLsizei -> Ptr GLint -> IO ())
glInstrumentsBufferSGIXFunPtr = unsafePerformIO (getProcAddress "glInstrumentsBufferSGIX")

{-# NOINLINE glInstrumentsBufferSGIXFunPtr #-}

-- | Usage: @'glPollInstrumentsSGIX' marker_p@
--
-- The length of @marker_p@ should be @1@.


glPollInstrumentsSGIX :: MonadIO m => Ptr GLint -> m GLint
glPollInstrumentsSGIX = ffiPtrintIOint glPollInstrumentsSGIXFunPtr

glPollInstrumentsSGIXFunPtr :: FunPtr (Ptr GLint -> IO GLint)
glPollInstrumentsSGIXFunPtr = unsafePerformIO (getProcAddress "glPollInstrumentsSGIX")

{-# NOINLINE glPollInstrumentsSGIXFunPtr #-}

-- | Usage: @'glReadInstrumentsSGIX' marker@


glReadInstrumentsSGIX :: MonadIO m => GLint -> m ()
glReadInstrumentsSGIX = ffiintIOV glReadInstrumentsSGIXFunPtr

glReadInstrumentsSGIXFunPtr :: FunPtr (GLint -> IO ())
glReadInstrumentsSGIXFunPtr = unsafePerformIO (getProcAddress "glReadInstrumentsSGIX")

{-# NOINLINE glReadInstrumentsSGIXFunPtr #-}

-- | Usage: @'glStartInstrumentsSGIX'@


glStartInstrumentsSGIX :: MonadIO m => m ()
glStartInstrumentsSGIX = ffiIOV glStartInstrumentsSGIXFunPtr

glStartInstrumentsSGIXFunPtr :: FunPtr (IO ())
glStartInstrumentsSGIXFunPtr = unsafePerformIO (getProcAddress "glStartInstrumentsSGIX")

{-# NOINLINE glStartInstrumentsSGIXFunPtr #-}

-- | Usage: @'glStopInstrumentsSGIX' marker@


glStopInstrumentsSGIX :: MonadIO m => GLint -> m ()
glStopInstrumentsSGIX = ffiintIOV glStopInstrumentsSGIXFunPtr

glStopInstrumentsSGIXFunPtr :: FunPtr (GLint -> IO ())
glStopInstrumentsSGIXFunPtr = unsafePerformIO (getProcAddress "glStopInstrumentsSGIX")

{-# NOINLINE glStopInstrumentsSGIXFunPtr #-}

pattern GL_INSTRUMENT_BUFFER_POINTER_SGIX  :: (Eq a, Num a) => a

pattern GL_INSTRUMENT_BUFFER_POINTER_SGIX = 0x8180

pattern GL_INSTRUMENT_MEASUREMENTS_SGIX  :: (Eq a, Num a) => a

pattern GL_INSTRUMENT_MEASUREMENTS_SGIX = 0x8181