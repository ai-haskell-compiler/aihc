-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.DebugOutput (
  -- * Extension Support
    gl_AMD_debug_output

  -- * GL_AMD_debug_output
  , glDebugMessageCallbackAMD
  , glDebugMessageEnableAMD
  , glDebugMessageInsertAMD
  , glGetDebugMessageLogAMD
  , pattern GL_DEBUG_CATEGORY_API_ERROR_AMD
  , pattern GL_DEBUG_CATEGORY_APPLICATION_AMD
  , pattern GL_DEBUG_CATEGORY_DEPRECATION_AMD
  , pattern GL_DEBUG_CATEGORY_OTHER_AMD
  , pattern GL_DEBUG_CATEGORY_PERFORMANCE_AMD
  , pattern GL_DEBUG_CATEGORY_SHADER_COMPILER_AMD
  , pattern GL_DEBUG_CATEGORY_UNDEFINED_BEHAVIOR_AMD
  , pattern GL_DEBUG_CATEGORY_WINDOW_SYSTEM_AMD
  , pattern GL_DEBUG_LOGGED_MESSAGES_AMD
  , pattern GL_DEBUG_SEVERITY_HIGH_AMD
  , pattern GL_DEBUG_SEVERITY_LOW_AMD
  , pattern GL_DEBUG_SEVERITY_MEDIUM_AMD
  , pattern GL_MAX_DEBUG_LOGGED_MESSAGES_AMD
  , pattern GL_MAX_DEBUG_MESSAGE_LENGTH_AMD
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/debug_output.txt GL_AMD_debug_output> extension is available.

gl_AMD_debug_output :: Bool
gl_AMD_debug_output = member "GL_AMD_debug_output" extensions
{-# NOINLINE gl_AMD_debug_output #-}

-- | Usage: @'glDebugMessageCallbackAMD' callback userParam@


glDebugMessageCallbackAMD :: MonadIO m => GLDEBUGPROCAMD -> Ptr () -> m ()
glDebugMessageCallbackAMD = ffiDEBUGPROCAMDPtrVIOV glDebugMessageCallbackAMDFunPtr

glDebugMessageCallbackAMDFunPtr :: FunPtr (GLDEBUGPROCAMD -> Ptr () -> IO ())
glDebugMessageCallbackAMDFunPtr = unsafePerformIO (getProcAddress "glDebugMessageCallbackAMD")

{-# NOINLINE glDebugMessageCallbackAMDFunPtr #-}

-- | Usage: @'glDebugMessageEnableAMD' category severity count ids enabled@
--
-- The parameter @enabled@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @ids@ should be @count@.


glDebugMessageEnableAMD :: MonadIO m => GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> m ()
glDebugMessageEnableAMD = ffienumenumsizeiPtruintbooleanIOV glDebugMessageEnableAMDFunPtr

glDebugMessageEnableAMDFunPtr :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
glDebugMessageEnableAMDFunPtr = unsafePerformIO (getProcAddress "glDebugMessageEnableAMD")

{-# NOINLINE glDebugMessageEnableAMDFunPtr #-}

-- | Usage: @'glDebugMessageInsertAMD' category severity id length buf@
--
-- The length of @buf@ should be @length@.


glDebugMessageInsertAMD :: MonadIO m => GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> m ()
glDebugMessageInsertAMD = ffienumenumuintsizeiPtrcharIOV glDebugMessageInsertAMDFunPtr

glDebugMessageInsertAMDFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
glDebugMessageInsertAMDFunPtr = unsafePerformIO (getProcAddress "glDebugMessageInsertAMD")

{-# NOINLINE glDebugMessageInsertAMDFunPtr #-}

-- | Usage: @'glGetDebugMessageLogAMD' count bufsize categories severities ids lengths message@
--
-- The length of @categories@ should be @count@.
--
-- The length of @severities@ should be @count@.
--
-- The length of @ids@ should be @count@.
--
-- The length of @lengths@ should be @count@.
--
-- The length of @message@ should be @bufsize@.


glGetDebugMessageLogAMD :: MonadIO m => GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> m GLuint
glGetDebugMessageLogAMD = ffiuintsizeiPtrenumPtruintPtruintPtrsizeiPtrcharIOuint glGetDebugMessageLogAMDFunPtr

glGetDebugMessageLogAMDFunPtr :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
glGetDebugMessageLogAMDFunPtr = unsafePerformIO (getProcAddress "glGetDebugMessageLogAMD")

{-# NOINLINE glGetDebugMessageLogAMDFunPtr #-}

pattern GL_DEBUG_CATEGORY_API_ERROR_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_API_ERROR_AMD = 0x9149

pattern GL_DEBUG_CATEGORY_APPLICATION_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_APPLICATION_AMD = 0x914F

pattern GL_DEBUG_CATEGORY_DEPRECATION_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_DEPRECATION_AMD = 0x914B

pattern GL_DEBUG_CATEGORY_OTHER_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_OTHER_AMD = 0x9150

pattern GL_DEBUG_CATEGORY_PERFORMANCE_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_PERFORMANCE_AMD = 0x914D

pattern GL_DEBUG_CATEGORY_SHADER_COMPILER_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_SHADER_COMPILER_AMD = 0x914E

pattern GL_DEBUG_CATEGORY_UNDEFINED_BEHAVIOR_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_UNDEFINED_BEHAVIOR_AMD = 0x914C

pattern GL_DEBUG_CATEGORY_WINDOW_SYSTEM_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_CATEGORY_WINDOW_SYSTEM_AMD = 0x914A

pattern GL_DEBUG_LOGGED_MESSAGES_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_LOGGED_MESSAGES_AMD = 0x9145

pattern GL_DEBUG_SEVERITY_HIGH_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_HIGH_AMD = 0x9146

pattern GL_DEBUG_SEVERITY_LOW_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_LOW_AMD = 0x9148

pattern GL_DEBUG_SEVERITY_MEDIUM_AMD  :: (Eq a, Num a) => a

pattern GL_DEBUG_SEVERITY_MEDIUM_AMD = 0x9147

pattern GL_MAX_DEBUG_LOGGED_MESSAGES_AMD  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_LOGGED_MESSAGES_AMD = 0x9144

pattern GL_MAX_DEBUG_MESSAGE_LENGTH_AMD  :: (Eq a, Num a) => a

pattern GL_MAX_DEBUG_MESSAGE_LENGTH_AMD = 0x9143