-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DebugLabel (
  -- * Extension Support
    gl_EXT_debug_label

  -- * GL_EXT_debug_label
  , glGetObjectLabelEXT
  , glLabelObjectEXT
  , pattern GL_BUFFER_OBJECT_EXT
  , pattern GL_PROGRAM_OBJECT_EXT
  , pattern GL_PROGRAM_PIPELINE_OBJECT_EXT
  , pattern GL_QUERY_OBJECT_EXT
  , pattern GL_SAMPLER
  , pattern GL_SHADER_OBJECT_EXT
  , pattern GL_TRANSFORM_FEEDBACK
  , pattern GL_VERTEX_ARRAY_OBJECT_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_debug_label.txt GL_EXT_debug_label> extension is available.

gl_EXT_debug_label :: Bool
gl_EXT_debug_label = member "GL_EXT_debug_label" extensions
{-# NOINLINE gl_EXT_debug_label #-}

-- | Usage: @'glGetObjectLabelEXT' type object bufSize length label@
--
-- The length of @length@ should be @1@.
--
-- The length of @label@ should be @bufSize@.


glGetObjectLabelEXT :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> m ()
glGetObjectLabelEXT = ffienumuintsizeiPtrsizeiPtrcharIOV glGetObjectLabelEXTFunPtr

glGetObjectLabelEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
glGetObjectLabelEXTFunPtr = unsafePerformIO (getProcAddress "glGetObjectLabelEXT")

{-# NOINLINE glGetObjectLabelEXTFunPtr #-}

-- | Usage: @'glLabelObjectEXT' type object length label@


glLabelObjectEXT :: MonadIO m => GLenum -> GLuint -> GLsizei -> Ptr GLchar -> m ()
glLabelObjectEXT = ffienumuintsizeiPtrcharIOV glLabelObjectEXTFunPtr

glLabelObjectEXTFunPtr :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
glLabelObjectEXTFunPtr = unsafePerformIO (getProcAddress "glLabelObjectEXT")

{-# NOINLINE glLabelObjectEXTFunPtr #-}

pattern GL_BUFFER_OBJECT_EXT  :: (Eq a, Num a) => a

pattern GL_BUFFER_OBJECT_EXT = 0x9151

pattern GL_PROGRAM_OBJECT_EXT  :: (Eq a, Num a) => a

pattern GL_PROGRAM_OBJECT_EXT = 0x8B40

pattern GL_PROGRAM_PIPELINE_OBJECT_EXT  :: (Eq a, Num a) => a

pattern GL_PROGRAM_PIPELINE_OBJECT_EXT = 0x8A4F

pattern GL_QUERY_OBJECT_EXT  :: (Eq a, Num a) => a

pattern GL_QUERY_OBJECT_EXT = 0x9153

pattern GL_SHADER_OBJECT_EXT  :: (Eq a, Num a) => a

pattern GL_SHADER_OBJECT_EXT = 0x8B48

pattern GL_VERTEX_ARRAY_OBJECT_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_OBJECT_EXT = 0x9154