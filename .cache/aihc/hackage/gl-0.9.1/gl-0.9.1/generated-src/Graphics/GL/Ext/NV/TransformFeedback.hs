-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TransformFeedback (
  -- * Extension Support
    gl_NV_transform_feedback

  -- * GL_NV_transform_feedback
  , glActiveVaryingNV
  , glBeginTransformFeedbackNV
  , glBindBufferBaseNV
  , glBindBufferOffsetNV
  , glBindBufferRangeNV
  , glEndTransformFeedbackNV
  , glGetActiveVaryingNV
  , glGetTransformFeedbackVaryingNV
  , glGetVaryingLocationNV
  , glTransformFeedbackAttribsNV
  , glTransformFeedbackStreamAttribsNV
  , glTransformFeedbackVaryingsNV
  , pattern GL_ACTIVE_VARYINGS_NV
  , pattern GL_ACTIVE_VARYING_MAX_LENGTH_NV
  , pattern GL_BACK_PRIMARY_COLOR_NV
  , pattern GL_BACK_SECONDARY_COLOR_NV
  , pattern GL_CLIP_DISTANCE_NV
  , pattern GL_GENERIC_ATTRIB_NV
  , pattern GL_INTERLEAVED_ATTRIBS_NV
  , pattern GL_LAYER_NV
  , pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV
  , pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV
  , pattern GL_NEXT_BUFFER_NV
  , pattern GL_PRIMITIVES_GENERATED_NV
  , pattern GL_PRIMITIVE_ID_NV
  , pattern GL_RASTERIZER_DISCARD_NV
  , pattern GL_SEPARATE_ATTRIBS_NV
  , pattern GL_SKIP_COMPONENTS1_NV
  , pattern GL_SKIP_COMPONENTS2_NV
  , pattern GL_SKIP_COMPONENTS3_NV
  , pattern GL_SKIP_COMPONENTS4_NV
  , pattern GL_TEXTURE_COORD_NV
  , pattern GL_TRANSFORM_FEEDBACK_ATTRIBS_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV
  , pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_NV
  , pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV
  , pattern GL_TRANSFORM_FEEDBACK_RECORD_NV
  , pattern GL_TRANSFORM_FEEDBACK_VARYINGS_NV
  , pattern GL_VERTEX_ID_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/transform_feedback.txt GL_NV_transform_feedback> extension is available.

gl_NV_transform_feedback :: Bool
gl_NV_transform_feedback = member "GL_NV_transform_feedback" extensions
{-# NOINLINE gl_NV_transform_feedback #-}

-- | Usage: @'glActiveVaryingNV' program name@
--
-- The length of @name@ should be @COMPSIZE(name)@.


glActiveVaryingNV :: MonadIO m => GLuint -> Ptr GLchar -> m ()
glActiveVaryingNV = ffiuintPtrcharIOV glActiveVaryingNVFunPtr

glActiveVaryingNVFunPtr :: FunPtr (GLuint -> Ptr GLchar -> IO ())
glActiveVaryingNVFunPtr = unsafePerformIO (getProcAddress "glActiveVaryingNV")

{-# NOINLINE glActiveVaryingNVFunPtr #-}

-- | Usage: @'glBeginTransformFeedbackNV' primitiveMode@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBeginTransformFeedback'.


glBeginTransformFeedbackNV :: MonadIO m => GLenum -> m ()
glBeginTransformFeedbackNV = ffienumIOV glBeginTransformFeedbackNVFunPtr

glBeginTransformFeedbackNVFunPtr :: FunPtr (GLenum -> IO ())
glBeginTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glBeginTransformFeedbackNV")

{-# NOINLINE glBeginTransformFeedbackNVFunPtr #-}

-- | Usage: @'glBindBufferBaseNV' target index buffer@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindBufferBase'.


glBindBufferBaseNV :: MonadIO m => GLenum -> GLuint -> GLuint -> m ()
glBindBufferBaseNV = ffienumuintuintIOV glBindBufferBaseNVFunPtr

glBindBufferBaseNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
glBindBufferBaseNVFunPtr = unsafePerformIO (getProcAddress "glBindBufferBaseNV")

{-# NOINLINE glBindBufferBaseNVFunPtr #-}

-- | Usage: @'glBindBufferOffsetNV' target index buffer offset@
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- This command is an alias for 'Graphics.GL.Ext.EXT.TransformFeedback.glBindBufferOffsetEXT'.


glBindBufferOffsetNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLintptr -> m ()
glBindBufferOffsetNV = ffienumuintuintintptrIOV glBindBufferOffsetNVFunPtr

glBindBufferOffsetNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ())
glBindBufferOffsetNVFunPtr = unsafePerformIO (getProcAddress "glBindBufferOffsetNV")

{-# NOINLINE glBindBufferOffsetNVFunPtr #-}

-- | Usage: @'glBindBufferRangeNV' target index buffer offset size@
--
-- The parameter @offset@ is a @BufferOffset@.
--
-- The parameter @size@ is a @BufferSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindBufferRange'.


glBindBufferRangeNV :: MonadIO m => GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> m ()
glBindBufferRangeNV = ffienumuintuintintptrsizeiptrIOV glBindBufferRangeNVFunPtr

glBindBufferRangeNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
glBindBufferRangeNVFunPtr = unsafePerformIO (getProcAddress "glBindBufferRangeNV")

{-# NOINLINE glBindBufferRangeNVFunPtr #-}

-- | Usage: @'glEndTransformFeedbackNV'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glEndTransformFeedback'.


glEndTransformFeedbackNV :: MonadIO m => m ()
glEndTransformFeedbackNV = ffiIOV glEndTransformFeedbackNVFunPtr

glEndTransformFeedbackNVFunPtr :: FunPtr (IO ())
glEndTransformFeedbackNVFunPtr = unsafePerformIO (getProcAddress "glEndTransformFeedbackNV")

{-# NOINLINE glEndTransformFeedbackNVFunPtr #-}

-- | Usage: @'glGetActiveVaryingNV' program index bufSize length size type name@
--
-- The length of @length@ should be @1@.
--
-- The length of @size@ should be @1@.
--
-- The length of @type@ should be @1@.
--
-- The length of @name@ should be @COMPSIZE(program,index,bufSize)@.


glGetActiveVaryingNV :: MonadIO m => GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> m ()
glGetActiveVaryingNV = ffiuintuintsizeiPtrsizeiPtrsizeiPtrenumPtrcharIOV glGetActiveVaryingNVFunPtr

glGetActiveVaryingNVFunPtr :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
glGetActiveVaryingNVFunPtr = unsafePerformIO (getProcAddress "glGetActiveVaryingNV")

{-# NOINLINE glGetActiveVaryingNVFunPtr #-}

-- | Usage: @'glGetTransformFeedbackVaryingNV' program index location@
--
-- The length of @location@ should be @1@.


glGetTransformFeedbackVaryingNV :: MonadIO m => GLuint -> GLuint -> Ptr GLint -> m ()
glGetTransformFeedbackVaryingNV = ffiuintuintPtrintIOV glGetTransformFeedbackVaryingNVFunPtr

glGetTransformFeedbackVaryingNVFunPtr :: FunPtr (GLuint -> GLuint -> Ptr GLint -> IO ())
glGetTransformFeedbackVaryingNVFunPtr = unsafePerformIO (getProcAddress "glGetTransformFeedbackVaryingNV")

{-# NOINLINE glGetTransformFeedbackVaryingNVFunPtr #-}

-- | Usage: @'glGetVaryingLocationNV' program name@
--
-- The length of @name@ should be @COMPSIZE(name)@.


glGetVaryingLocationNV :: MonadIO m => GLuint -> Ptr GLchar -> m GLint
glGetVaryingLocationNV = ffiuintPtrcharIOint glGetVaryingLocationNVFunPtr

glGetVaryingLocationNVFunPtr :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
glGetVaryingLocationNVFunPtr = unsafePerformIO (getProcAddress "glGetVaryingLocationNV")

{-# NOINLINE glGetVaryingLocationNVFunPtr #-}

-- | Usage: @'glTransformFeedbackAttribsNV' count attribs bufferMode@
--
-- The length of @attribs@ should be @COMPSIZE(count)@.


glTransformFeedbackAttribsNV :: MonadIO m => GLsizei -> Ptr GLint -> GLenum -> m ()
glTransformFeedbackAttribsNV = ffisizeiPtrintenumIOV glTransformFeedbackAttribsNVFunPtr

glTransformFeedbackAttribsNVFunPtr :: FunPtr (GLsizei -> Ptr GLint -> GLenum -> IO ())
glTransformFeedbackAttribsNVFunPtr = unsafePerformIO (getProcAddress "glTransformFeedbackAttribsNV")

{-# NOINLINE glTransformFeedbackAttribsNVFunPtr #-}

-- | Usage: @'glTransformFeedbackStreamAttribsNV' count attribs nbuffers bufstreams bufferMode@
--
-- The length of @attribs@ should be @count@.
--
-- The length of @bufstreams@ should be @nbuffers@.


glTransformFeedbackStreamAttribsNV :: MonadIO m => GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> m ()
glTransformFeedbackStreamAttribsNV = ffisizeiPtrintsizeiPtrintenumIOV glTransformFeedbackStreamAttribsNVFunPtr

glTransformFeedbackStreamAttribsNVFunPtr :: FunPtr (GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> IO ())
glTransformFeedbackStreamAttribsNVFunPtr = unsafePerformIO (getProcAddress "glTransformFeedbackStreamAttribsNV")

{-# NOINLINE glTransformFeedbackStreamAttribsNVFunPtr #-}

-- | Usage: @'glTransformFeedbackVaryingsNV' program count locations bufferMode@
--
-- The length of @locations@ should be @count@.


glTransformFeedbackVaryingsNV :: MonadIO m => GLuint -> GLsizei -> Ptr GLint -> GLenum -> m ()
glTransformFeedbackVaryingsNV = ffiuintsizeiPtrintenumIOV glTransformFeedbackVaryingsNVFunPtr

glTransformFeedbackVaryingsNVFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> GLenum -> IO ())
glTransformFeedbackVaryingsNVFunPtr = unsafePerformIO (getProcAddress "glTransformFeedbackVaryingsNV")

{-# NOINLINE glTransformFeedbackVaryingsNVFunPtr #-}

pattern GL_ACTIVE_VARYINGS_NV  :: (Eq a, Num a) => a

pattern GL_ACTIVE_VARYINGS_NV = 0x8C81

pattern GL_ACTIVE_VARYING_MAX_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_ACTIVE_VARYING_MAX_LENGTH_NV = 0x8C82

pattern GL_BACK_PRIMARY_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_BACK_PRIMARY_COLOR_NV = 0x8C77

pattern GL_BACK_SECONDARY_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_BACK_SECONDARY_COLOR_NV = 0x8C78

pattern GL_CLIP_DISTANCE_NV  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE_NV = 0x8C7A

pattern GL_GENERIC_ATTRIB_NV  :: (Eq a, Num a) => a

pattern GL_GENERIC_ATTRIB_NV = 0x8C7D

pattern GL_INTERLEAVED_ATTRIBS_NV  :: (Eq a, Num a) => a

pattern GL_INTERLEAVED_ATTRIBS_NV = 0x8C8C

pattern GL_LAYER_NV  :: (Eq a, Num a) => a

pattern GL_LAYER_NV = 0x8DAA

pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS_NV = 0x8C8A

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS_NV = 0x8C8B

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS_NV = 0x8C80

pattern GL_NEXT_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_NEXT_BUFFER_NV = -2

pattern GL_PRIMITIVES_GENERATED_NV  :: (Eq a, Num a) => a

pattern GL_PRIMITIVES_GENERATED_NV = 0x8C87

pattern GL_PRIMITIVE_ID_NV  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_ID_NV = 0x8C7C

pattern GL_RASTERIZER_DISCARD_NV  :: (Eq a, Num a) => a

pattern GL_RASTERIZER_DISCARD_NV = 0x8C89

pattern GL_SEPARATE_ATTRIBS_NV  :: (Eq a, Num a) => a

pattern GL_SEPARATE_ATTRIBS_NV = 0x8C8D

pattern GL_SKIP_COMPONENTS1_NV  :: (Eq a, Num a) => a

pattern GL_SKIP_COMPONENTS1_NV = -6

pattern GL_SKIP_COMPONENTS2_NV  :: (Eq a, Num a) => a

pattern GL_SKIP_COMPONENTS2_NV = -5

pattern GL_SKIP_COMPONENTS3_NV  :: (Eq a, Num a) => a

pattern GL_SKIP_COMPONENTS3_NV = -4

pattern GL_SKIP_COMPONENTS4_NV  :: (Eq a, Num a) => a

pattern GL_SKIP_COMPONENTS4_NV = -3

pattern GL_TEXTURE_COORD_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_NV = 0x8C79

pattern GL_TRANSFORM_FEEDBACK_ATTRIBS_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_ATTRIBS_NV = 0x8C7E

pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_BINDING_NV = 0x8C8F

pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_MODE_NV = 0x8C7F

pattern GL_TRANSFORM_FEEDBACK_BUFFER_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_NV = 0x8C8E

pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_SIZE_NV = 0x8C85

pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_BUFFER_START_NV = 0x8C84

pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN_NV = 0x8C88

pattern GL_TRANSFORM_FEEDBACK_RECORD_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_RECORD_NV = 0x8C86

pattern GL_TRANSFORM_FEEDBACK_VARYINGS_NV  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_FEEDBACK_VARYINGS_NV = 0x8C83

pattern GL_VERTEX_ID_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ID_NV = 0x8C7B