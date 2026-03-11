-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.Debug (
  -- * Extension Support
    gl_KHR_debug

  -- * GL_KHR_debug
  , glDebugMessageCallback
  , glDebugMessageCallbackKHR
  , glDebugMessageControl
  , glDebugMessageControlKHR
  , glDebugMessageInsert
  , glDebugMessageInsertKHR
  , glGetDebugMessageLog
  , glGetDebugMessageLogKHR
  , glGetObjectLabel
  , glGetObjectLabelKHR
  , glGetObjectPtrLabel
  , glGetObjectPtrLabelKHR
  , glGetPointerv
  , glGetPointervKHR
  , glObjectLabel
  , glObjectLabelKHR
  , glObjectPtrLabel
  , glObjectPtrLabelKHR
  , glPopDebugGroup
  , glPopDebugGroupKHR
  , glPushDebugGroup
  , glPushDebugGroupKHR
  , pattern GL_BUFFER
  , pattern GL_BUFFER_KHR
  , pattern GL_CONTEXT_FLAG_DEBUG_BIT
  , pattern GL_CONTEXT_FLAG_DEBUG_BIT_KHR
  , pattern GL_DEBUG_CALLBACK_FUNCTION
  , pattern GL_DEBUG_CALLBACK_FUNCTION_KHR
  , pattern GL_DEBUG_CALLBACK_USER_PARAM
  , pattern GL_DEBUG_CALLBACK_USER_PARAM_KHR
  , pattern GL_DEBUG_GROUP_STACK_DEPTH
  , pattern GL_DEBUG_GROUP_STACK_DEPTH_KHR
  , pattern GL_DEBUG_LOGGED_MESSAGES
  , pattern GL_DEBUG_LOGGED_MESSAGES_KHR
  , pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH
  , pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_KHR
  , pattern GL_DEBUG_OUTPUT
  , pattern GL_DEBUG_OUTPUT_KHR
  , pattern GL_DEBUG_OUTPUT_SYNCHRONOUS
  , pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_KHR
  , pattern GL_DEBUG_SEVERITY_HIGH
  , pattern GL_DEBUG_SEVERITY_HIGH_KHR
  , pattern GL_DEBUG_SEVERITY_LOW
  , pattern GL_DEBUG_SEVERITY_LOW_KHR
  , pattern GL_DEBUG_SEVERITY_MEDIUM
  , pattern GL_DEBUG_SEVERITY_MEDIUM_KHR
  , pattern GL_DEBUG_SEVERITY_NOTIFICATION
  , pattern GL_DEBUG_SEVERITY_NOTIFICATION_KHR
  , pattern GL_DEBUG_SOURCE_API
  , pattern GL_DEBUG_SOURCE_API_KHR
  , pattern GL_DEBUG_SOURCE_APPLICATION
  , pattern GL_DEBUG_SOURCE_APPLICATION_KHR
  , pattern GL_DEBUG_SOURCE_OTHER
  , pattern GL_DEBUG_SOURCE_OTHER_KHR
  , pattern GL_DEBUG_SOURCE_SHADER_COMPILER
  , pattern GL_DEBUG_SOURCE_SHADER_COMPILER_KHR
  , pattern GL_DEBUG_SOURCE_THIRD_PARTY
  , pattern GL_DEBUG_SOURCE_THIRD_PARTY_KHR
  , pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM
  , pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_KHR
  , pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR
  , pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_KHR
  , pattern GL_DEBUG_TYPE_ERROR
  , pattern GL_DEBUG_TYPE_ERROR_KHR
  , pattern GL_DEBUG_TYPE_MARKER
  , pattern GL_DEBUG_TYPE_MARKER_KHR
  , pattern GL_DEBUG_TYPE_OTHER
  , pattern GL_DEBUG_TYPE_OTHER_KHR
  , pattern GL_DEBUG_TYPE_PERFORMANCE
  , pattern GL_DEBUG_TYPE_PERFORMANCE_KHR
  , pattern GL_DEBUG_TYPE_POP_GROUP
  , pattern GL_DEBUG_TYPE_POP_GROUP_KHR
  , pattern GL_DEBUG_TYPE_PORTABILITY
  , pattern GL_DEBUG_TYPE_PORTABILITY_KHR
  , pattern GL_DEBUG_TYPE_PUSH_GROUP
  , pattern GL_DEBUG_TYPE_PUSH_GROUP_KHR
  , pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR
  , pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_KHR
  , pattern GL_DISPLAY_LIST
  , pattern GL_MAX_DEBUG_GROUP_STACK_DEPTH
  , pattern GL_MAX_DEBUG_GROUP_STACK_DEPTH_KHR
  , pattern GL_MAX_DEBUG_LOGGED_MESSAGES
  , pattern GL_MAX_DEBUG_LOGGED_MESSAGES_KHR
  , pattern GL_MAX_DEBUG_MESSAGE_LENGTH
  , pattern GL_MAX_DEBUG_MESSAGE_LENGTH_KHR
  , pattern GL_MAX_LABEL_LENGTH
  , pattern GL_MAX_LABEL_LENGTH_KHR
  , pattern GL_PROGRAM
  , pattern GL_PROGRAM_KHR
  , pattern GL_PROGRAM_PIPELINE
  , pattern GL_PROGRAM_PIPELINE_KHR
  , pattern GL_QUERY
  , pattern GL_QUERY_KHR
  , pattern GL_SAMPLER
  , pattern GL_SAMPLER_KHR
  , pattern GL_SHADER
  , pattern GL_SHADER_KHR
  , pattern GL_STACK_OVERFLOW
  , pattern GL_STACK_OVERFLOW_KHR
  , pattern GL_STACK_UNDERFLOW
  , pattern GL_STACK_UNDERFLOW_KHR
  , pattern GL_VERTEX_ARRAY
  , pattern GL_VERTEX_ARRAY_KHR
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/debug.txt GL_KHR_debug> extension is available.

gl_KHR_debug :: Bool
gl_KHR_debug = member "GL_KHR_debug" extensions
{-# NOINLINE gl_KHR_debug #-}

-- | Usage: @'glDebugMessageCallbackKHR' callback userParam@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDebugMessageCallback'.


glDebugMessageCallbackKHR :: MonadIO m => GLDEBUGPROCKHR -> Ptr () -> m ()
glDebugMessageCallbackKHR = ffiDEBUGPROCKHRPtrVIOV glDebugMessageCallbackKHRFunPtr

glDebugMessageCallbackKHRFunPtr :: FunPtr (GLDEBUGPROCKHR -> Ptr () -> IO ())
glDebugMessageCallbackKHRFunPtr = unsafePerformIO (getProcAddress "glDebugMessageCallbackKHR")

{-# NOINLINE glDebugMessageCallbackKHRFunPtr #-}

-- | Usage: @'glDebugMessageControlKHR' source type severity count ids enabled@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDebugMessageControl'.


glDebugMessageControlKHR :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> m ()
glDebugMessageControlKHR = ffienumenumenumsizeiPtruintbooleanIOV glDebugMessageControlKHRFunPtr

glDebugMessageControlKHRFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
glDebugMessageControlKHRFunPtr = unsafePerformIO (getProcAddress "glDebugMessageControlKHR")

{-# NOINLINE glDebugMessageControlKHRFunPtr #-}

-- | Usage: @'glDebugMessageInsertKHR' source type id severity length buf@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDebugMessageInsert'.


glDebugMessageInsertKHR :: MonadIO m => GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> m ()
glDebugMessageInsertKHR = ffienumenumuintenumsizeiPtrcharIOV glDebugMessageInsertKHRFunPtr

glDebugMessageInsertKHRFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
glDebugMessageInsertKHRFunPtr = unsafePerformIO (getProcAddress "glDebugMessageInsertKHR")

{-# NOINLINE glDebugMessageInsertKHRFunPtr #-}

-- | Usage: @'glGetDebugMessageLogKHR' count bufSize sources types ids severities lengths messageLog@
--
-- The length of @sources@ should be @count@.
--
-- The length of @types@ should be @count@.
--
-- The length of @ids@ should be @count@.
--
-- The length of @severities@ should be @count@.
--
-- The length of @lengths@ should be @count@.
--
-- The length of @messageLog@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetDebugMessageLog'.


glGetDebugMessageLogKHR :: MonadIO m => GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> m GLuint
glGetDebugMessageLogKHR = ffiuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint glGetDebugMessageLogKHRFunPtr

glGetDebugMessageLogKHRFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
glGetDebugMessageLogKHRFunPtr = unsafePerformIO (getProcAddress "glGetDebugMessageLogKHR")

{-# NOINLINE glGetDebugMessageLogKHRFunPtr #-}

-- | Usage: @'glGetObjectLabelKHR' identifier name bufSize length label@
--
-- The length of @label@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetObjectLabel'.


glGetObjectLabelKHR :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetObjectLabelKHR = ffienumuintsizeiPtrsizeiPtrcharIOV glGetObjectLabelKHRFunPtr

glGetObjectLabelKHRFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetObjectLabelKHRFunPtr = unsafePerformIO (getProcAddress "glGetObjectLabelKHR")

{-# NOINLINE glGetObjectLabelKHRFunPtr #-}

-- | Usage: @'glGetObjectPtrLabelKHR' ptr bufSize length label@
--
-- The length of @length@ should be @1@.
--
-- The length of @label@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetObjectPtrLabel'.


glGetObjectPtrLabelKHR :: MonadIO m => Ptr () -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetObjectPtrLabelKHR = ffiPtrVsizeiPtrsizeiPtrcharIOV glGetObjectPtrLabelKHRFunPtr

glGetObjectPtrLabelKHRFunPtr :: FunPtr (Ptr () -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetObjectPtrLabelKHRFunPtr = unsafePerformIO (getProcAddress "glGetObjectPtrLabelKHR")

{-# NOINLINE glGetObjectPtrLabelKHRFunPtr #-}

-- | Usage: @'glGetPointervKHR' pname params@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetPointerv'.


glGetPointervKHR :: MonadIO m => GLenum -> Ptr (Ptr ()) -> m ()
glGetPointervKHR = ffienumPtrPtrVIOV glGetPointervKHRFunPtr

glGetPointervKHRFunPtr :: FunPtr (GLenum -> Ptr (Ptr ()) -> IO ())
glGetPointervKHRFunPtr = unsafePerformIO (getProcAddress "glGetPointervKHR")

{-# NOINLINE glGetPointervKHRFunPtr #-}

-- | Usage: @'glObjectLabelKHR' identifier name length label@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glObjectLabel'.


glObjectLabelKHR :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLchar -> m ()
glObjectLabelKHR = ffienumuintsizeiPtrcharIOV glObjectLabelKHRFunPtr

glObjectLabelKHRFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
glObjectLabelKHRFunPtr = unsafePerformIO (getProcAddress "glObjectLabelKHR")

{-# NOINLINE glObjectLabelKHRFunPtr #-}

-- | Usage: @'glObjectPtrLabelKHR' ptr length label@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glObjectPtrLabel'.


glObjectPtrLabelKHR :: MonadIO m => Ptr () -> GLsizei -> Ptr GLchar -> m ()
glObjectPtrLabelKHR = ffiPtrVsizeiPtrcharIOV glObjectPtrLabelKHRFunPtr

glObjectPtrLabelKHRFunPtr :: FunPtr (Ptr () -> GLsizei -> Ptr GLchar -> IO ())
glObjectPtrLabelKHRFunPtr = unsafePerformIO (getProcAddress "glObjectPtrLabelKHR")

{-# NOINLINE glObjectPtrLabelKHRFunPtr #-}

-- | Usage: @'glPopDebugGroupKHR'@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPopDebugGroup'.


glPopDebugGroupKHR :: MonadIO m => m ()
glPopDebugGroupKHR = ffiIOV glPopDebugGroupKHRFunPtr

glPopDebugGroupKHRFunPtr :: FunPtr (IO ())
glPopDebugGroupKHRFunPtr = unsafePerformIO (getProcAddress "glPopDebugGroupKHR")

{-# NOINLINE glPopDebugGroupKHRFunPtr #-}

-- | Usage: @'glPushDebugGroupKHR' source id length message@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPushDebugGroup'.


glPushDebugGroupKHR :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLchar -> m ()
glPushDebugGroupKHR = ffienumuintsizeiPtrcharIOV glPushDebugGroupKHRFunPtr

glPushDebugGroupKHRFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
glPushDebugGroupKHRFunPtr = unsafePerformIO (getProcAddress "glPushDebugGroupKHR")

{-# NOINLINE glPushDebugGroupKHRFunPtr #-}

pattern GL_BUFFER_KHR  :: (Eq a, Num a) => a

pattern GL_BUFFER_KHR = 0x82E0

pattern GL_CONTEXT_FLAG_DEBUG_BIT_KHR  :: (Eq a, Num a) => a

pattern GL_CONTEXT_FLAG_DEBUG_BIT_KHR = 0x00000002

pattern GL_DEBUG_CALLBACK_FUNCTION_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_CALLBACK_FUNCTION_KHR = 0x8244

pattern GL_DEBUG_CALLBACK_USER_PARAM_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_CALLBACK_USER_PARAM_KHR = 0x8245

pattern GL_DEBUG_GROUP_STACK_DEPTH_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_GROUP_STACK_DEPTH_KHR = 0x826D

pattern GL_DEBUG_LOGGED_MESSAGES_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_LOGGED_MESSAGES_KHR = 0x9145

pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_KHR = 0x8243

pattern GL_DEBUG_OUTPUT_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_OUTPUT_KHR = 0x92E0

pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_KHR = 0x8242

pattern GL_DEBUG_SEVERITY_HIGH_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_HIGH_KHR = 0x9146

pattern GL_DEBUG_SEVERITY_LOW_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_LOW_KHR = 0x9148

pattern GL_DEBUG_SEVERITY_MEDIUM_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_MEDIUM_KHR = 0x9147

pattern GL_DEBUG_SEVERITY_NOTIFICATION_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_NOTIFICATION_KHR = 0x826B

pattern GL_DEBUG_SOURCE_API_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_API_KHR = 0x8246

pattern GL_DEBUG_SOURCE_APPLICATION_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_APPLICATION_KHR = 0x824A

pattern GL_DEBUG_SOURCE_OTHER_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_OTHER_KHR = 0x824B

pattern GL_DEBUG_SOURCE_SHADER_COMPILER_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_SHADER_COMPILER_KHR = 0x8248

pattern GL_DEBUG_SOURCE_THIRD_PARTY_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_THIRD_PARTY_KHR = 0x8249

pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_KHR = 0x8247

pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_KHR = 0x824D

pattern GL_DEBUG_TYPE_ERROR_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_ERROR_KHR = 0x824C

pattern GL_DEBUG_TYPE_MARKER_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_MARKER_KHR = 0x8268

pattern GL_DEBUG_TYPE_OTHER_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_OTHER_KHR = 0x8251

pattern GL_DEBUG_TYPE_PERFORMANCE_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_PERFORMANCE_KHR = 0x8250

pattern GL_DEBUG_TYPE_POP_GROUP_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_POP_GROUP_KHR = 0x826A

pattern GL_DEBUG_TYPE_PORTABILITY_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_PORTABILITY_KHR = 0x824F

pattern GL_DEBUG_TYPE_PUSH_GROUP_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_PUSH_GROUP_KHR = 0x8269

pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_KHR  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_KHR = 0x824E

pattern GL_MAX_DEBUG_GROUP_STACK_DEPTH_KHR  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_GROUP_STACK_DEPTH_KHR = 0x826C

pattern GL_MAX_DEBUG_LOGGED_MESSAGES_KHR  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_LOGGED_MESSAGES_KHR = 0x9144

pattern GL_MAX_DEBUG_MESSAGE_LENGTH_KHR  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_MESSAGE_LENGTH_KHR = 0x9143

pattern GL_MAX_LABEL_LENGTH_KHR  :: (Eq a, Num a) => a

pattern GL_MAX_LABEL_LENGTH_KHR = 0x82E8

pattern GL_PROGRAM_KHR  :: (Eq a, Num a) => a

pattern GL_PROGRAM_KHR = 0x82E2

pattern GL_PROGRAM_PIPELINE_KHR  :: (Eq a, Num a) => a

pattern GL_PROGRAM_PIPELINE_KHR = 0x82E4

pattern GL_QUERY_KHR  :: (Eq a, Num a) => a

pattern GL_QUERY_KHR = 0x82E3

pattern GL_SAMPLER_KHR  :: (Eq a, Num a) => a

pattern GL_SAMPLER_KHR = 0x82E6

pattern GL_SHADER_KHR  :: (Eq a, Num a) => a

pattern GL_SHADER_KHR = 0x82E1

pattern GL_STACK_OVERFLOW_KHR  :: (Eq a, Num a) => a

pattern GL_STACK_OVERFLOW_KHR = 0x0503

pattern GL_STACK_UNDERFLOW_KHR  :: (Eq a, Num a) => a

pattern GL_STACK_UNDERFLOW_KHR = 0x0504

pattern GL_VERTEX_ARRAY_KHR  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_KHR = 0x8074