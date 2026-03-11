-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TransformFeedback2 (
  -- * Extension Support
    gl_NV_transform_feedback2

  -- * GL_NV_transform_feedback2
  , glBindTransformFeedbackNV
  , glDeleteTransformFeedbacksNV
  , glDrawTransformFeedbackNV
  , glGenTransformFeedbacksNV
  , glIsTransformFeedbackNV
  , glPauseTransformFeedbackNV
  , glResumeTransformFeedbackNV
  , pattern GL_TRANSFORM_FEEDBACK_BINDING_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED_NV
  , pattern GL_TRANSFORM_FEEDBACK_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/transform_feedback2.txt GL_NV_transform_feedback2> extension is available.

gl_NV_transform_feedback2 :: Bool
gl_NV_transform_feedback2 = member "GL_NV_transform_feedback2" extensions
{-# NOINLINE gl_NV_transform_feedback2 #-}

-- | Usage: @'glBindTransformFeedbackNV' target id@
--
-- The parameter @target@ is a @BufferTargetARB@.


glBindTransformFeedbackNV :: MonadIO m => GLenum -> GLuint -> m ()
glBindTransformFeedbackNV = ffienumuintIOV glBindTransformFeedbackNVFunPtr

glBindTransformFeedbackNVFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glBindTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glBindTransformFeedbackNV")

{-# NOINLINE glBindTransformFeedbackNVFunPtr #-}

-- | Usage: @'glDeleteTransformFeedbacksNV' n ids@
--
-- The length of @ids@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDeleteTransformFeedbacks'.


glDeleteTransformFeedbacksNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteTransformFeedbacksNV = ffisizeiPtruintIOV glDeleteTransformFeedbacksNVFunPtr

glDeleteTransformFeedbacksNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteTransformFeedbacksNVFunPtr = unsafePerformIO (getProcAddress "glDeleteTransformFeedbacksNV")

{-# NOINLINE glDeleteTransformFeedbacksNVFunPtr #-}

-- | Usage: @'glDrawTransformFeedbackNV' mode id@
--
-- The parameter @mode@ is a @PrimitiveType@, one of: 'Graphics.GL.Internal.Shared.GL_LINES', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_LINE_LOOP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_LINE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_LINE_STRIP_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_PATCHES', 'Graphics.GL.Ext.EXT.TessellationShader.GL_PATCHES_EXT', 'Graphics.GL.Internal.Shared.GL_POINTS', 'Graphics.GL.Internal.Shared.GL_POLYGON', 'Graphics.GL.Internal.Shared.GL_QUADS', 'Graphics.GL.Ext.EXT.TessellationShader.GL_QUADS_EXT', 'Graphics.GL.Internal.Shared.GL_QUAD_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLES', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLES_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLES_ADJACENCY_EXT', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_FAN', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY', 'Graphics.GL.Ext.ARB.GeometryShader4.GL_TRIANGLE_STRIP_ADJACENCY_ARB', 'Graphics.GL.Internal.Shared.GL_TRIANGLE_STRIP_ADJACENCY_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDrawTransformFeedback'.


glDrawTransformFeedbackNV :: MonadIO m => GLenum -> GLuint -> m ()
glDrawTransformFeedbackNV = ffienumuintIOV glDrawTransformFeedbackNVFunPtr

glDrawTransformFeedbackNVFunPtr :: FunPtr (GLenum -> GLuint -> IO ())
glDrawTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glDrawTransformFeedbackNV")

{-# NOINLINE glDrawTransformFeedbackNVFunPtr #-}

-- | Usage: @'glGenTransformFeedbacksNV' n ids@
--
-- The length of @ids@ should be @n@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGenTransformFeedbacks'.


glGenTransformFeedbacksNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glGenTransformFeedbacksNV = ffisizeiPtruintIOV glGenTransformFeedbacksNVFunPtr

glGenTransformFeedbacksNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glGenTransformFeedbacksNVFunPtr = unsafePerformIO (getProcAddress "glGenTransformFeedbacksNV")

{-# NOINLINE glGenTransformFeedbacksNVFunPtr #-}

-- | Usage: @'glIsTransformFeedbackNV' id@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glIsTransformFeedback'.


glIsTransformFeedbackNV :: MonadIO m => GLuint -> m GLboolean
glIsTransformFeedbackNV = ffiuintIOboolean glIsTransformFeedbackNVFunPtr

glIsTransformFeedbackNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glIsTransformFeedbackNV")

{-# NOINLINE glIsTransformFeedbackNVFunPtr #-}

-- | Usage: @'glPauseTransformFeedbackNV'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPauseTransformFeedback'.


glPauseTransformFeedbackNV :: MonadIO m => m ()
glPauseTransformFeedbackNV = ffiIOV glPauseTransformFeedbackNVFunPtr

glPauseTransformFeedbackNVFunPtr :: FunPtr (IO ())
glPauseTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glPauseTransformFeedbackNV")

{-# NOINLINE glPauseTransformFeedbackNVFunPtr #-}

-- | Usage: @'glResumeTransformFeedbackNV'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glResumeTransformFeedback'.


glResumeTransformFeedbackNV :: MonadIO m => m ()
glResumeTransformFeedbackNV = ffiIOV glResumeTransformFeedbackNVFunPtr

glResumeTransformFeedbackNVFunPtr :: FunPtr (IO ())
glResumeTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glResumeTransformFeedbackNV")

{-# NOINLINE glResumeTransformFeedbackNVFunPtr #-}

pattern GL_TRANSFORM_FEEDBACK_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BINDING_NV = 0x8E25

pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE_NV = 0x8E24

pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED_NV = 0x8E23

pattern GL_TRANSFORM_FEEDBACK_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_NV = 0x8E22