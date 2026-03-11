-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.DebugOutput (
  -- * Extension Support
    gl_ARB_debug_output

  -- * GL_ARB_debug_output
  , glDebugMessageCallbackARB
  , glDebugMessageControlARB
  , glDebugMessageInsertARB
  , glGetDebugMessageLogARB
  , pattern GL_DEBUG_CALLBACK_FUNCTION_ARB
  , pattern GL_DEBUG_CALLBACK_USER_PARAM_ARB
  , pattern GL_DEBUG_LOGGED_MESSAGES_ARB
  , pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB
  , pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB
  , pattern GL_DEBUG_SEVERITY_HIGH_ARB
  , pattern GL_DEBUG_SEVERITY_LOW_ARB
  , pattern GL_DEBUG_SEVERITY_MEDIUM_ARB
  , pattern GL_DEBUG_SOURCE_API_ARB
  , pattern GL_DEBUG_SOURCE_APPLICATION_ARB
  , pattern GL_DEBUG_SOURCE_OTHER_ARB
  , pattern GL_DEBUG_SOURCE_SHADER_COMPILER_ARB
  , pattern GL_DEBUG_SOURCE_THIRD_PARTY_ARB
  , pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB
  , pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB
  , pattern GL_DEBUG_TYPE_ERROR_ARB
  , pattern GL_DEBUG_TYPE_OTHER_ARB
  , pattern GL_DEBUG_TYPE_PERFORMANCE_ARB
  , pattern GL_DEBUG_TYPE_PORTABILITY_ARB
  , pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB
  , pattern GL_MAX_DEBUG_LOGGED_MESSAGES_ARB
  , pattern GL_MAX_DEBUG_MESSAGE_LENGTH_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/debug_output.txt GL_ARB_debug_output> extension is available.

gl_ARB_debug_output :: Bool
gl_ARB_debug_output = member "GL_ARB_debug_output" extensions
{-# NOINLINE gl_ARB_debug_output #-}

-- | Usage: @'glDebugMessageCallbackARB' callback userParam@
--
-- The length of @userParam@ should be @COMPSIZE(callback)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDebugMessageCallback'.


glDebugMessageCallbackARB :: MonadIO m => GLDEBUGPROCARB -> Ptr () -> m ()
glDebugMessageCallbackARB = ffiDEBUGPROCARBPtrVIOV glDebugMessageCallbackARBFunPtr

glDebugMessageCallbackARBFunPtr :: FunPtr (GLDEBUGPROCARB -> Ptr () -> IO ())
glDebugMessageCallbackARBFunPtr = unsafePerformIO (getProcAddress "glDebugMessageCallbackARB")

{-# NOINLINE glDebugMessageCallbackARBFunPtr #-}

-- | Usage: @'glDebugMessageControlARB' source type severity count ids enabled@
--
-- The parameter @enabled@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @ids@ should be @count@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDebugMessageControl'.


glDebugMessageControlARB :: MonadIO m => GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> m ()
glDebugMessageControlARB = ffienumenumenumsizeiPtruintbooleanIOV glDebugMessageControlARBFunPtr

glDebugMessageControlARBFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
glDebugMessageControlARBFunPtr = unsafePerformIO (getProcAddress "glDebugMessageControlARB")

{-# NOINLINE glDebugMessageControlARBFunPtr #-}

-- | Usage: @'glDebugMessageInsertARB' source type id severity length buf@
--
-- The length of @buf@ should be @length@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glDebugMessageInsert'.


glDebugMessageInsertARB :: MonadIO m => GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> m ()
glDebugMessageInsertARB = ffienumenumuintenumsizeiPtrcharIOV glDebugMessageInsertARBFunPtr

glDebugMessageInsertARBFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
glDebugMessageInsertARBFunPtr = unsafePerformIO (getProcAddress "glDebugMessageInsertARB")

{-# NOINLINE glDebugMessageInsertARBFunPtr #-}

-- | Usage: @'glGetDebugMessageLogARB' count bufSize sources types ids severities lengths messageLog@
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


glGetDebugMessageLogARB :: MonadIO m => GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> m GLuint
glGetDebugMessageLogARB = ffiuintsizeiPtrenumPtrenumPtruintPtrenumPtrsizeiPtrcharIOuint glGetDebugMessageLogARBFunPtr

glGetDebugMessageLogARBFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
glGetDebugMessageLogARBFunPtr = unsafePerformIO (getProcAddress "glGetDebugMessageLogARB")

{-# NOINLINE glGetDebugMessageLogARBFunPtr #-}

pattern GL_DEBUG_CALLBACK_FUNCTION_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_CALLBACK_FUNCTION_ARB = 0x8244

pattern GL_DEBUG_CALLBACK_USER_PARAM_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_CALLBACK_USER_PARAM_ARB = 0x8245

pattern GL_DEBUG_LOGGED_MESSAGES_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_LOGGED_MESSAGES_ARB = 0x9145

pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB = 0x8243

pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB = 0x8242

pattern GL_DEBUG_SEVERITY_HIGH_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_HIGH_ARB = 0x9146

pattern GL_DEBUG_SEVERITY_LOW_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_LOW_ARB = 0x9148

pattern GL_DEBUG_SEVERITY_MEDIUM_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_MEDIUM_ARB = 0x9147

pattern GL_DEBUG_SOURCE_API_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_API_ARB = 0x8246

pattern GL_DEBUG_SOURCE_APPLICATION_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_APPLICATION_ARB = 0x824A

pattern GL_DEBUG_SOURCE_OTHER_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_OTHER_ARB = 0x824B

pattern GL_DEBUG_SOURCE_SHADER_COMPILER_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_SHADER_COMPILER_ARB = 0x8248

pattern GL_DEBUG_SOURCE_THIRD_PARTY_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_THIRD_PARTY_ARB = 0x8249

pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB = 0x8247

pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB = 0x824D

pattern GL_DEBUG_TYPE_ERROR_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_ERROR_ARB = 0x824C

pattern GL_DEBUG_TYPE_OTHER_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_OTHER_ARB = 0x8251

pattern GL_DEBUG_TYPE_PERFORMANCE_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_PERFORMANCE_ARB = 0x8250

pattern GL_DEBUG_TYPE_PORTABILITY_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_PORTABILITY_ARB = 0x824F

pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB  :: (Eq a, Num a) => a

pattern GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB = 0x824E

pattern GL_MAX_DEBUG_LOGGED_MESSAGES_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_LOGGED_MESSAGES_ARB = 0x9144

pattern GL_MAX_DEBUG_MESSAGE_LENGTH_ARB  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_MESSAGE_LENGTH_ARB = 0x9143