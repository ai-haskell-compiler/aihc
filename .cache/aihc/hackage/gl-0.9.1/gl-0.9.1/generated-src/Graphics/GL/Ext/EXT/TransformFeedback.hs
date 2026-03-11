-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TransformFeedback (
  -- * Extension Support
    gl_EXT_transform_feedback

  -- * GL_EXT_transform_feedback
  , glBeginTransformFeedbackEXT
  , glBindBufferBaseEXT
  , glBindBufferOffsetEXT
  , glBindBufferRangeEXT
  , glEndTransformFeedbackEXT
  , glGetTransformFeedbackVaryingEXT
  , glTransformFeedbackVaryingsEXT
  , pattern GL_INTERLEAVED_ATTRIBS_EXT
  , pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT
  , pattern GL_PRIMITIVES_GENERATED_EXT
  , pattern GL_RASTERIZER_DISCARD_EXT
  , pattern GL_SEPARATE_ATTRIBS_EXT
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_EXT
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT
  , pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT
  , pattern GL_TRANSFORM_FEEDBACK_VARYINGS_EXT
  , pattern GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/transform_feedback.txt GL_EXT_transform_feedback> extension is available.

gl_EXT_transform_feedback :: Bool
gl_EXT_transform_feedback = member "GL_EXT_transform_feedback" extensions
{-# NOINLINE gl_EXT_transform_feedback #-}

-- | Usage: @'glBeginTransformFeedbackEXT' primitiveMode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBeginTransformFeedback'.


glBeginTransformFeedbackEXT :: MonadIO m => GLenum -> m ()
glBeginTransformFeedbackEXT = ffienumIOV glBeginTransformFeedbackEXTFunPtr

glBeginTransformFeedbackEXTFunPtr :: FunPtr (GLenum -> IO ())
glBeginTransformFeedbackEXTFunPtr = unsafePerformIO (getProcAddress "glBeginTransformFeedbackEXT")

{-# NOINLINE glBeginTransformFeedbackEXTFunPtr #-}

-- | Usage: @'glBindBufferBaseEXT' target index buffer@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindBufferBase'.


glBindBufferBaseEXT :: MonadIO m => GLenum -> GLuint -> GLuint -> m ()
glBindBufferBaseEXT = ffienumuintuintIOV glBindBufferBaseEXTFunPtr

glBindBufferBaseEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
glBindBufferBaseEXTFunPtr = unsafePerformIO (getProcAddress "glBindBufferBaseEXT")

{-# NOINLINE glBindBufferBaseEXTFunPtr #-}

-- | Usage: @'glBindBufferOffsetEXT' target index buffer offset@
--
-- The parameter @offset@ is a @BufferOffset@.


glBindBufferOffsetEXT :: MonadIO m => GLenum -> GLuint -> GLuint -> GLintptr -> m ()
glBindBufferOffsetEXT = ffienumuintuintintptrIOV glBindBufferOffsetEXTFunPtr

glBindBufferOffsetEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ())
glBindBufferOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glBindBufferOffsetEXT")

{-# NOINLINE glBindBufferOffsetEXTFunPtr #-}

-- | Usage: @'glBindBufferRangeEXT' target index buffer offset size@
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindBufferRange'.


glBindBufferRangeEXT :: MonadIO m => GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> m ()
glBindBufferRangeEXT = ffienumuintuintintptrsizeiptrIOV glBindBufferRangeEXTFunPtr

glBindBufferRangeEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
glBindBufferRangeEXTFunPtr = unsafePerformIO (getProcAddress "glBindBufferRangeEXT")

{-# NOINLINE glBindBufferRangeEXTFunPtr #-}

-- | Usage: @'glEndTransformFeedbackEXT'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEndTransformFeedback'.


glEndTransformFeedbackEXT :: MonadIO m => m ()
glEndTransformFeedbackEXT = ffiIOV glEndTransformFeedbackEXTFunPtr

glEndTransformFeedbackEXTFunPtr :: FunPtr (IO ())
glEndTransformFeedbackEXTFunPtr = unsafePerformIO (getProcAddress "glEndTransformFeedbackEXT")

{-# NOINLINE glEndTransformFeedbackEXTFunPtr #-}

-- | Usage: @'glGetTransformFeedbackVaryingEXT' program index bufSize length size type name@
--
-- The length of @length@ should be @1@.
--
-- The length of @size@ should be @1@.
--
-- The length of @type@ should be @1@.
--
-- The length of @name@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetTransformFeedbackVarying'.


glGetTransformFeedbackVaryingEXT :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> m ()
glGetTransformFeedbackVaryingEXT = ffiuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV glGetTransformFeedbackVaryingEXTFunPtr

glGetTransformFeedbackVaryingEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
glGetTransformFeedbackVaryingEXTFunPtr = unsafePerformIO (getProcAddress "glGetTransformFeedbackVaryingEXT")

{-# NOINLINE glGetTransformFeedbackVaryingEXTFunPtr #-}

-- | Usage: @'glTransformFeedbackVaryingsEXT' program count varyings bufferMode@
--
-- The length of @varyings@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTransformFeedbackVaryings'.


glTransformFeedbackVaryingsEXT :: MonadIO m => GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> m ()
glTransformFeedbackVaryingsEXT = ffiuintsizeiPtrPtrcharenumIOV glTransformFeedbackVaryingsEXTFunPtr

glTransformFeedbackVaryingsEXTFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> IO ())
glTransformFeedbackVaryingsEXTFunPtr = unsafePerformIO (getProcAddress "glTransformFeedbackVaryingsEXT")

{-# NOINLINE glTransformFeedbackVaryingsEXTFunPtr #-}

pattern GL_INTERLEAVED_ATTRIBS_EXT  :: (Eq a, Num a) => a

pattern GL_INTERLEAVED_ATTRIBS_EXT = 0x8C8C

pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_EXT = 0x8C8A

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_EXT = 0x8C8B

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_EXT = 0x8C80

pattern GL_RASTERIZER_DISCARD_EXT  :: (Eq a, Num a) => a

pattern GL_RASTERIZER_DISCARD_EXT = 0x8C89

pattern GL_SEPARATE_ATTRIBS_EXT  :: (Eq a, Num a) => a

pattern GL_SEPARATE_ATTRIBS_EXT = 0x8C8D

pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_EXT = 0x8C8F

pattern GL_TRANSFORM_FEEDBACK_BUFFER_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_EXT = 0x8C8E

pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_EXT = 0x8C7F

pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_EXT = 0x8C85

pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_EXT = 0x8C84

pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_EXT = 0x8C88

pattern GL_TRANSFORM_FEEDBACK_VARYINGS_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_VARYINGS_EXT = 0x8C83

pattern GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH_EXT = 0x8C76