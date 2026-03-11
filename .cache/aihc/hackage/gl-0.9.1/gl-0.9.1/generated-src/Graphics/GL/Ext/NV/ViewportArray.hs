-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ViewportArray (
  -- * Extension Support
    gl_NV_viewport_array

  -- * GL_NV_viewport_array
  , glDepthRangeArrayfvNV
  , glDepthRangeIndexedfNV
  , glDisableiNV
  , glEnableiNV
  , glGetFloati_vNV
  , glIsEnablediNV
  , glScissorArrayvNV
  , glScissorIndexedNV
  , glScissorIndexedvNV
  , glViewportArrayvNV
  , glViewportIndexedfNV
  , glViewportIndexedfvNV
  , pattern GL_DEPTH_RANGE
  , pattern GL_MAX_VIEWPORTS_NV
  , pattern GL_SCISSOR_BOX
  , pattern GL_SCISSOR_TEST
  , pattern GL_VIEWPORT
  , pattern GL_VIEWPORT_BOUNDS_RANGE_NV
  , pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX_NV
  , pattern GL_VIEWPORT_SUBPIXEL_BITS_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_NV_viewport_array extension is available.

gl_NV_viewport_array :: Bool
gl_NV_viewport_array = member "GL_NV_viewport_array" extensions
{-# NOINLINE gl_NV_viewport_array #-}

-- | Usage: @'glDepthRangeArrayfvNV' first count v@


glDepthRangeArrayfvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glDepthRangeArrayfvNV = ffiuintsizeiPtrfloatIOV glDepthRangeArrayfvNVFunPtr

glDepthRangeArrayfvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glDepthRangeArrayfvNVFunPtr = unsafePerformIO (getProcAddress "glDepthRangeArrayfvNV")

{-# NOINLINE glDepthRangeArrayfvNVFunPtr #-}

-- | Usage: @'glDepthRangeIndexedfNV' index n f@


glDepthRangeIndexedfNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> m ()
glDepthRangeIndexedfNV = ffiuintfloatfloatIOV glDepthRangeIndexedfNVFunPtr

glDepthRangeIndexedfNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
glDepthRangeIndexedfNVFunPtr = unsafePerformIO (getProcAddress "glDepthRangeIndexedfNV")

{-# NOINLINE glDepthRangeIndexedfNVFunPtr #-}

-- | Usage: @'glDisableiNV' target index@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDisablei'.


glDisableiNV :: MonadIO m => GLenum -> GLuint -> m ()
glDisableiNV = ffienumuintIOV glDisableiNVFunPtr

glDisableiNVFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glDisableiNVFunPtr = unsafePerformIO (getProcAddress "glDisableiNV")

{-# NOINLINE glDisableiNVFunPtr #-}

-- | Usage: @'glEnableiNV' target index@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEnablei'.


glEnableiNV :: MonadIO m => GLenum -> GLuint -> m ()
glEnableiNV = ffienumuintIOV glEnableiNVFunPtr

glEnableiNVFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glEnableiNVFunPtr = unsafePerformIO (getProcAddress "glEnableiNV")

{-# NOINLINE glEnableiNVFunPtr #-}

-- | Usage: @'glGetFloati_vNV' target index data@
--
-- The parameter @target@ is a @TypeEnum@.
--
-- The length of @data@ should be @COMPSIZE(target)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFloati_v'.


glGetFloati_vNV :: MonadIO m => GLenum -> GLuint -> Ptr GLfloat -> m ()
glGetFloati_vNV = ffienumuintPtrfloatIOV glGetFloati_vNVFunPtr

glGetFloati_vNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
glGetFloati_vNVFunPtr = unsafePerformIO (getProcAddress "glGetFloati_vNV")

{-# NOINLINE glGetFloati_vNVFunPtr #-}

-- | Usage: @'glIsEnablediNV' target index@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsEnabledi'.


glIsEnablediNV :: MonadIO m => GLenum -> GLuint -> m GLboolean
glIsEnablediNV = ffienumuintIOboolean glIsEnablediNVFunPtr

glIsEnablediNVFunPtr :: FunPtr (GLenum -> GLuint -> IO GLboolean)
glIsEnablediNVFunPtr = unsafePerformIO (getProcAddress "glIsEnablediNV")

{-# NOINLINE glIsEnablediNVFunPtr #-}

-- | Usage: @'glScissorArrayvNV' first count v@
--
-- The length of @v@ should be @COMPSIZE(count)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glScissorArrayv'.


glScissorArrayvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLint -> m ()
glScissorArrayvNV = ffiuintsizeiPtrintIOV glScissorArrayvNVFunPtr

glScissorArrayvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> IO ())
glScissorArrayvNVFunPtr = unsafePerformIO (getProcAddress "glScissorArrayvNV")

{-# NOINLINE glScissorArrayvNVFunPtr #-}

-- | Usage: @'glScissorIndexedNV' index left bottom width height@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glScissorIndexed'.


glScissorIndexedNV :: MonadIO m => GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> m ()
glScissorIndexedNV = ffiuintintintsizeisizeiIOV glScissorIndexedNVFunPtr

glScissorIndexedNVFunPtr :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
glScissorIndexedNVFunPtr = unsafePerformIO (getProcAddress "glScissorIndexedNV")

{-# NOINLINE glScissorIndexedNVFunPtr #-}

-- | Usage: @'glScissorIndexedvNV' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glScissorIndexedv'.


glScissorIndexedvNV :: MonadIO m => GLuint -> Ptr GLint -> m ()
glScissorIndexedvNV = ffiuintPtrintIOV glScissorIndexedvNVFunPtr

glScissorIndexedvNVFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glScissorIndexedvNVFunPtr = unsafePerformIO (getProcAddress "glScissorIndexedvNV")

{-# NOINLINE glScissorIndexedvNVFunPtr #-}

-- | Usage: @'glViewportArrayvNV' first count v@
--
-- The length of @v@ should be @COMPSIZE(count)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glViewportArrayv'.


glViewportArrayvNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLfloat -> m ()
glViewportArrayvNV = ffiuintsizeiPtrfloatIOV glViewportArrayvNVFunPtr

glViewportArrayvNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
glViewportArrayvNVFunPtr = unsafePerformIO (getProcAddress "glViewportArrayvNV")

{-# NOINLINE glViewportArrayvNVFunPtr #-}

-- | Usage: @'glViewportIndexedfNV' index x y w h@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glViewportIndexedf'.


glViewportIndexedfNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glViewportIndexedfNV = ffiuintfloatfloatfloatfloatIOV glViewportIndexedfNVFunPtr

glViewportIndexedfNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glViewportIndexedfNVFunPtr = unsafePerformIO (getProcAddress "glViewportIndexedfNV")

{-# NOINLINE glViewportIndexedfNVFunPtr #-}

-- | Usage: @'glViewportIndexedfvNV' index v@
--
-- The length of @v@ should be @4@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glViewportIndexedfv'.


glViewportIndexedfvNV :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glViewportIndexedfvNV = ffiuintPtrfloatIOV glViewportIndexedfvNVFunPtr

glViewportIndexedfvNVFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glViewportIndexedfvNVFunPtr = unsafePerformIO (getProcAddress "glViewportIndexedfvNV")

{-# NOINLINE glViewportIndexedfvNVFunPtr #-}

pattern GL_MAX_VIEWPORTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_VIEWPORTS_NV = 0x825B

pattern GL_VIEWPORT_BOUNDS_RANGE_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_BOUNDS_RANGE_NV = 0x825D

pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_INDEX_PROVOKING_VERTEX_NV = 0x825F

pattern GL_VIEWPORT_SUBPIXEL_BITS_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_SUBPIXEL_BITS_NV = 0x825C