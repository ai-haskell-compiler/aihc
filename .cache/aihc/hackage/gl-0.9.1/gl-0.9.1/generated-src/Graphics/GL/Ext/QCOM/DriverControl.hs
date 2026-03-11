-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.DriverControl (
  -- * Extension Support
    gl_QCOM_driver_control

  -- * GL_QCOM_driver_control
  , glDisableDriverControlQCOM
  , glEnableDriverControlQCOM
  , glGetDriverControlStringQCOM
  , glGetDriverControlsQCOM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_driver_control.txt GL_QCOM_driver_control> extension is available.

gl_QCOM_driver_control :: Bool
gl_QCOM_driver_control = member "GL_QCOM_driver_control" extensions
{-# NOINLINE gl_QCOM_driver_control #-}

-- | Usage: @'glDisableDriverControlQCOM' driverControl@


glDisableDriverControlQCOM :: MonadIO m => GLuint -> m ()
glDisableDriverControlQCOM = ffiuintIOV glDisableDriverControlQCOMFunPtr

glDisableDriverControlQCOMFunPtr :: FunPtr (GLuint -> IO ())
glDisableDriverControlQCOMFunPtr = unsafePerformIO (getProcAddress "glDisableDriverControlQCOM")

{-# NOINLINE glDisableDriverControlQCOMFunPtr #-}

-- | Usage: @'glEnableDriverControlQCOM' driverControl@


glEnableDriverControlQCOM :: MonadIO m => GLuint -> m ()
glEnableDriverControlQCOM = ffiuintIOV glEnableDriverControlQCOMFunPtr

glEnableDriverControlQCOMFunPtr :: FunPtr (GLuint -> IO ())
glEnableDriverControlQCOMFunPtr = unsafePerformIO (getProcAddress "glEnableDriverControlQCOM")

{-# NOINLINE glEnableDriverControlQCOMFunPtr #-}

-- | Usage: @'glGetDriverControlStringQCOM' driverControl bufSize length driverControlString@
--
-- The length of @driverControlString@ should be @bufSize@.


glGetDriverControlStringQCOM :: MonadIO m => GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetDriverControlStringQCOM = ffiuintsizeiPtrsizeiPtrcharIOV glGetDriverControlStringQCOMFunPtr

glGetDriverControlStringQCOMFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetDriverControlStringQCOMFunPtr = unsafePerformIO (getProcAddress "glGetDriverControlStringQCOM")

{-# NOINLINE glGetDriverControlStringQCOMFunPtr #-}

-- | Usage: @'glGetDriverControlsQCOM' num size driverControls@
--
-- The length of @driverControls@ should be @size@.


glGetDriverControlsQCOM :: MonadIO m => Ptr GLint -> GLsizei -> Ptr GLuint -> m ()
glGetDriverControlsQCOM = ffiPtrintsizeiPtruintIOV glGetDriverControlsQCOMFunPtr

glGetDriverControlsQCOMFunPtr :: FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
glGetDriverControlsQCOMFunPtr = unsafePerformIO (getProcAddress "glGetDriverControlsQCOM")

{-# NOINLINE glGetDriverControlsQCOMFunPtr #-}