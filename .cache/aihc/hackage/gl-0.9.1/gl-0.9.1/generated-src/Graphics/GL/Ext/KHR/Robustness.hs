-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.Robustness (
  -- * Extension Support
    gl_KHR_robustness

  -- * GL_KHR_robustness
  , glGetGraphicsResetStatus
  , glGetGraphicsResetStatusKHR
  , glGetnUniformfv
  , glGetnUniformfvKHR
  , glGetnUniformiv
  , glGetnUniformivKHR
  , glGetnUniformuiv
  , glGetnUniformuivKHR
  , glReadnPixels
  , glReadnPixelsKHR
  , pattern GL_CONTEXT_LOST
  , pattern GL_CONTEXT_LOST_KHR
  , pattern GL_CONTEXT_ROBUST_ACCESS
  , pattern GL_CONTEXT_ROBUST_ACCESS_KHR
  , pattern GL_GUILTY_CONTEXT_RESET
  , pattern GL_GUILTY_CONTEXT_RESET_KHR
  , pattern GL_INNOCENT_CONTEXT_RESET
  , pattern GL_INNOCENT_CONTEXT_RESET_KHR
  , pattern GL_LOSE_CONTEXT_ON_RESET
  , pattern GL_LOSE_CONTEXT_ON_RESET_KHR
  , pattern GL_NO_ERROR
  , pattern GL_NO_RESET_NOTIFICATION
  , pattern GL_NO_RESET_NOTIFICATION_KHR
  , pattern GL_RESET_NOTIFICATION_STRATEGY
  , pattern GL_RESET_NOTIFICATION_STRATEGY_KHR
  , pattern GL_UNKNOWN_CONTEXT_RESET
  , pattern GL_UNKNOWN_CONTEXT_RESET_KHR
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/robustness.txt GL_KHR_robustness> extension is available.

gl_KHR_robustness :: Bool
gl_KHR_robustness = member "GL_KHR_robustness" extensions
{-# NOINLINE gl_KHR_robustness #-}

-- | Usage: @'glGetGraphicsResetStatusKHR'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetGraphicsResetStatus'.


glGetGraphicsResetStatusKHR :: MonadIO m => m GLenum
glGetGraphicsResetStatusKHR = ffiIOenum glGetGraphicsResetStatusKHRFunPtr

glGetGraphicsResetStatusKHRFunPtr :: FunPtr (IO GLenum)
glGetGraphicsResetStatusKHRFunPtr = unsafePerformIO (getProcAddress "glGetGraphicsResetStatusKHR")

{-# NOINLINE glGetGraphicsResetStatusKHRFunPtr #-}

-- | Usage: @'glGetnUniformfvKHR' program location bufSize params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetnUniformfv'.


glGetnUniformfvKHR :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLfloat -> m ()
glGetnUniformfvKHR = ffiuintintsizeiPtrfloatIOV glGetnUniformfvKHRFunPtr

glGetnUniformfvKHRFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
glGetnUniformfvKHRFunPtr = unsafePerformIO (getProcAddress "glGetnUniformfvKHR")

{-# NOINLINE glGetnUniformfvKHRFunPtr #-}

-- | Usage: @'glGetnUniformivKHR' program location bufSize params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetnUniformiv'.


glGetnUniformivKHR :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint -> m ()
glGetnUniformivKHR = ffiuintintsizeiPtrintIOV glGetnUniformivKHRFunPtr

glGetnUniformivKHRFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
glGetnUniformivKHRFunPtr = unsafePerformIO (getProcAddress "glGetnUniformivKHR")

{-# NOINLINE glGetnUniformivKHRFunPtr #-}

-- | Usage: @'glGetnUniformuivKHR' program location bufSize params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetnUniformuiv'.


glGetnUniformuivKHR :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLuint -> m ()
glGetnUniformuivKHR = ffiuintintsizeiPtruintIOV glGetnUniformuivKHRFunPtr

glGetnUniformuivKHRFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
glGetnUniformuivKHRFunPtr = unsafePerformIO (getProcAddress "glGetnUniformuivKHR")

{-# NOINLINE glGetnUniformuivKHRFunPtr #-}

-- | Usage: @'glReadnPixelsKHR' x y width height format type bufSize data@
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @data@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glReadnPixels'.


glReadnPixelsKHR :: MonadIO m => GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glReadnPixelsKHR = ffiintintsizeisizeienumenumsizeiPtrVIOV glReadnPixelsKHRFunPtr

glReadnPixelsKHRFunPtr :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glReadnPixelsKHRFunPtr = unsafePerformIO (getProcAddress "glReadnPixelsKHR")

{-# NOINLINE glReadnPixelsKHRFunPtr #-}

pattern GL_CONTEXT_LOST_KHR  :: (Eq a, Num a) => a

pattern GL_CONTEXT_LOST_KHR = 0x0507

pattern GL_CONTEXT_ROBUST_ACCESS_KHR  :: (Eq a, Num a) => a

pattern GL_CONTEXT_ROBUST_ACCESS_KHR = 0x90F3

pattern GL_GUILTY_CONTEXT_RESET_KHR  :: (Eq a, Num a) => a

pattern GL_GUILTY_CONTEXT_RESET_KHR = 0x8253

pattern GL_INNOCENT_CONTEXT_RESET_KHR  :: (Eq a, Num a) => a

pattern GL_INNOCENT_CONTEXT_RESET_KHR = 0x8254

pattern GL_LOSE_CONTEXT_ON_RESET_KHR  :: (Eq a, Num a) => a

pattern GL_LOSE_CONTEXT_ON_RESET_KHR = 0x8252

pattern GL_NO_RESET_NOTIFICATION_KHR  :: (Eq a, Num a) => a

pattern GL_NO_RESET_NOTIFICATION_KHR = 0x8261

pattern GL_RESET_NOTIFICATION_STRATEGY_KHR  :: (Eq a, Num a) => a

pattern GL_RESET_NOTIFICATION_STRATEGY_KHR = 0x8256

pattern GL_UNKNOWN_CONTEXT_RESET_KHR  :: (Eq a, Num a) => a

pattern GL_UNKNOWN_CONTEXT_RESET_KHR = 0x8255