-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.ViewportArray (
  -- * Extension Support
    gl_OES_viewport_array

  -- * GL_OES_viewport_array
  , glDepthRangeArrayfvOES
  , glDepthRangeIndexedfOES
  , glDisableiOES
  , glEnableiOES
  , glGetFloati_vOES
  , glIsEnablediOES
  , glScissorArrayvOES
  , glScissorIndexedOES
  , glScissorIndexedvOES
  , glViewportArrayvOES
  , glViewportIndexedfOES
  , glViewportIndexedfvOES
  , pattern GL_DEPTH_RANGE
  , pattern GL_MAX_VIEWPORTS_OES
  , pattern GL_SCISSOR_BOX
  , pattern GL_SCISSOR_TEST
  , pattern GL_VIEWPORT
  , pattern GL_VIEWPORT_BOUNDS_RANGE_OES
  , pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX_OES
  , pattern GL_VIEWPORT_SUBPIXEL_BITS_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/OES/OES_viewport_array.txt GL_OES_viewport_array> extension is available.

gl_OES_viewport_array :: Bool
gl_OES_viewport_array = member "GL_OES_viewport_array" extensions
{-# NOINLINE gl_OES_viewport_array #-}

-- | Usage: @'glDepthRangeArrayfvOES' first count v@


glDepthRangeArrayfvOES :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glDepthRangeArrayfvOES = ffiuintsizeiPtrfloatIOV glDepthRangeArrayfvOESFunPtr

glDepthRangeArrayfvOESFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glDepthRangeArrayfvOESFunPtr = unsafePerformIO (getProcAddress "glDepthRangeArrayfvOES")

{-# NOINLINE glDepthRangeArrayfvOESFunPtr #-}

-- | Usage: @'glDepthRangeIndexedfOES' index n f@


glDepthRangeIndexedfOES :: MonadIO m => GLuint -> GLfloat -> GLfloat -> m ()
glDepthRangeIndexedfOES = ffiuintfloatfloatIOV glDepthRangeIndexedfOESFunPtr

glDepthRangeIndexedfOESFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
glDepthRangeIndexedfOESFunPtr = unsafePerformIO (getProcAddress "glDepthRangeIndexedfOES")

{-# NOINLINE glDepthRangeIndexedfOESFunPtr #-}

-- | Usage: @'glGetFloati_vOES' target index data@
--
-- The parameter @target@ is a @TypeEnum@.
--
-- The length of @data@ should be @COMPSIZE(target)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFloati_v'.


glGetFloati_vOES :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glGetFloati_vOES = ffienumuintPtrfloatIOV glGetFloati_vOESFunPtr

glGetFloati_vOESFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glGetFloati_vOESFunPtr = unsafePerformIO (getProcAddress "glGetFloati_vOES")

{-# NOINLINE glGetFloati_vOESFunPtr #-}

-- | Usage: @'glScissorArrayvOES' first count v@
--
-- The length of @v@ should be @COMPSIZE(count)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glScissorArrayv'.


glScissorArrayvOES :: MonadIO m => GLuint -> GLsizei -> Ptr GLint -> m ()
glScissorArrayvOES = ffiuintsizeiPtrintIOV glScissorArrayvOESFunPtr

glScissorArrayvOESFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> IO ())
glScissorArrayvOESFunPtr = unsafePerformIO (getProcAddress "glScissorArrayvOES")

{-# NOINLINE glScissorArrayvOESFunPtr #-}

-- | Usage: @'glScissorIndexedOES' index left bottom width height@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glScissorIndexed'.


glScissorIndexedOES :: MonadIO m => GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glScissorIndexedOES = ffiuintintintsizeisizeiIOV glScissorIndexedOESFunPtr

glScissorIndexedOESFunPtr :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glScissorIndexedOESFunPtr = unsafePerformIO (getProcAddress "glScissorIndexedOES")

{-# NOINLINE glScissorIndexedOESFunPtr #-}

-- | Usage: @'glScissorIndexedvOES' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glScissorIndexedv'.


glScissorIndexedvOES :: MonadIO m => GLuint -> Ptr GLint -> m ()
glScissorIndexedvOES = ffiuintPtrintIOV glScissorIndexedvOESFunPtr

glScissorIndexedvOESFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glScissorIndexedvOESFunPtr = unsafePerformIO (getProcAddress "glScissorIndexedvOES")

{-# NOINLINE glScissorIndexedvOESFunPtr #-}

-- | Usage: @'glViewportArrayvOES' first count v@
--
-- The length of @v@ should be @COMPSIZE(count)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glViewportArrayv'.


glViewportArrayvOES :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glViewportArrayvOES = ffiuintsizeiPtrfloatIOV glViewportArrayvOESFunPtr

glViewportArrayvOESFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glViewportArrayvOESFunPtr = unsafePerformIO (getProcAddress "glViewportArrayvOES")

{-# NOINLINE glViewportArrayvOESFunPtr #-}

-- | Usage: @'glViewportIndexedfOES' index x y w h@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glViewportIndexedf'.


glViewportIndexedfOES :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glViewportIndexedfOES = ffiuintfloatfloatfloatfloatIOV glViewportIndexedfOESFunPtr

glViewportIndexedfOESFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glViewportIndexedfOESFunPtr = unsafePerformIO (getProcAddress "glViewportIndexedfOES")

{-# NOINLINE glViewportIndexedfOESFunPtr #-}

-- | Usage: @'glViewportIndexedfvOES' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glViewportIndexedfv'.


glViewportIndexedfvOES :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glViewportIndexedfvOES = ffiuintPtrfloatIOV glViewportIndexedfvOESFunPtr

glViewportIndexedfvOESFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glViewportIndexedfvOESFunPtr = unsafePerformIO (getProcAddress "glViewportIndexedfvOES")

{-# NOINLINE glViewportIndexedfvOESFunPtr #-}

pattern GL_MAX_VIEWPORTS_OES  :: (Eq a, Num a) => a

pattern GL_MAX_VIEWPORTS_OES = 0x825B

pattern GL_VIEWPORT_BOUNDS_RANGE_OES  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_BOUNDS_RANGE_OES = 0x825D

pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX_OES  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX_OES = 0x825F

pattern GL_VIEWPORT_SUBPIXEL_BITS_OES  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SUBPIXEL_BITS_OES = 0x825C