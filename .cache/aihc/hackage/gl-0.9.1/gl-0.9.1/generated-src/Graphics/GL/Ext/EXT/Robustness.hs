-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Robustness (
  -- * Extension Support
    gl_EXT_robustness

  -- * GL_EXT_robustness
  , glGetGraphicsResetStatusEXT
  , glGetnUniformfvEXT
  , glGetnUniformivEXT
  , glReadnPixelsEXT
  , pattern GL_CONTEXT_ROBUST_ACCESS_EXT
  , pattern GL_GUILTY_CONTEXT_RESET_EXT
  , pattern GL_INNOCENT_CONTEXT_RESET_EXT
  , pattern GL_LOSE_CONTEXT_ON_RESET_EXT
  , pattern GL_NO_ERROR
  , pattern GL_NO_RESET_NOTIFICATION_EXT
  , pattern GL_RESET_NOTIFICATION_STRATEGY_EXT
  , pattern GL_UNKNOWN_CONTEXT_RESET_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_robustness.txt GL_EXT_robustness> extension is available.

gl_EXT_robustness :: Bool
gl_EXT_robustness = member "GL_EXT_robustness" extensions
{-# NOINLINE gl_EXT_robustness #-}

-- | Usage: @'glGetGraphicsResetStatusEXT'@


glGetGraphicsResetStatusEXT :: MonadIO m => m GLenum
glGetGraphicsResetStatusEXT = ffiIOenum glGetGraphicsResetStatusEXTFunPtr

glGetGraphicsResetStatusEXTFunPtr :: FunPtr (IO GLenum)
glGetGraphicsResetStatusEXTFunPtr = unsafePerformIO (getProcAddress "glGetGraphicsResetStatusEXT")

{-# NOINLINE glGetGraphicsResetStatusEXTFunPtr #-}

-- | Usage: @'glGetnUniformfvEXT' program location bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetnUniformfvEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLfloat -> m ()
glGetnUniformfvEXT = ffiuintintsizeiPtrfloatIOV glGetnUniformfvEXTFunPtr

glGetnUniformfvEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
glGetnUniformfvEXTFunPtr = unsafePerformIO (getProcAddress "glGetnUniformfvEXT")

{-# NOINLINE glGetnUniformfvEXTFunPtr #-}

-- | Usage: @'glGetnUniformivEXT' program location bufSize params@
--
-- The length of @params@ should be @bufSize@.


glGetnUniformivEXT :: MonadIO m => GLuint -> GLint -> GLsizei -> Ptr GLint -> m ()
glGetnUniformivEXT = ffiuintintsizeiPtrintIOV glGetnUniformivEXTFunPtr

glGetnUniformivEXTFunPtr :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
glGetnUniformivEXTFunPtr = unsafePerformIO (getProcAddress "glGetnUniformivEXT")

{-# NOINLINE glGetnUniformivEXTFunPtr #-}

-- | Usage: @'glReadnPixelsEXT' x y width height format type bufSize data@
--
-- The length of @data@ should be @bufSize@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glReadnPixels'.


glReadnPixelsEXT :: MonadIO m => GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> m ()
glReadnPixelsEXT = ffiintintsizeisizeienumenumsizeiPtrVIOV glReadnPixelsEXTFunPtr

glReadnPixelsEXTFunPtr :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> IO ())
glReadnPixelsEXTFunPtr = unsafePerformIO (getProcAddress "glReadnPixelsEXT")

{-# NOINLINE glReadnPixelsEXTFunPtr #-}

pattern GL_CONTEXT_ROBUST_ACCESS_EXT  :: (Eq a, Num a) => a

pattern GL_CONTEXT_ROBUST_ACCESS_EXT = 0x90F3

pattern GL_GUILTY_CONTEXT_RESET_EXT  :: (Eq a, Num a) => a

pattern GL_GUILTY_CONTEXT_RESET_EXT = 0x8253

pattern GL_INNOCENT_CONTEXT_RESET_EXT  :: (Eq a, Num a) => a

pattern GL_INNOCENT_CONTEXT_RESET_EXT = 0x8254

pattern GL_LOSE_CONTEXT_ON_RESET_EXT  :: (Eq a, Num a) => a

pattern GL_LOSE_CONTEXT_ON_RESET_EXT = 0x8252

pattern GL_NO_RESET_NOTIFICATION_EXT  :: (Eq a, Num a) => a

pattern GL_NO_RESET_NOTIFICATION_EXT = 0x8261

pattern GL_RESET_NOTIFICATION_STRATEGY_EXT  :: (Eq a, Num a) => a

pattern GL_RESET_NOTIFICATION_STRATEGY_EXT = 0x8256

pattern GL_UNKNOWN_CONTEXT_RESET_EXT  :: (Eq a, Num a) => a

pattern GL_UNKNOWN_CONTEXT_RESET_EXT = 0x8255