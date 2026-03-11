-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendFuncExtended (
  -- * Extension Support
    gl_EXT_blend_func_extended

  -- * GL_EXT_blend_func_extended
  , glBindFragDataLocationEXT
  , glBindFragDataLocationIndexedEXT
  , glGetFragDataIndexEXT
  , glGetProgramResourceLocationIndexEXT
  , pattern GL_LOCATION_INDEX_EXT
  , pattern GL_MAX_DUAL_SOURCE_DRAW_BUFFERS_EXT
  , pattern GL_ONE_MINUS_SRC1_ALPHA_EXT
  , pattern GL_ONE_MINUS_SRC1_COLOR_EXT
  , pattern GL_SRC1_ALPHA_EXT
  , pattern GL_SRC1_COLOR_EXT
  , pattern GL_SRC_ALPHA_SATURATE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_EXT_blend_func_extended extension is available.

gl_EXT_blend_func_extended :: Bool
gl_EXT_blend_func_extended = member "GL_EXT_blend_func_extended" extensions
{-# NOINLINE gl_EXT_blend_func_extended #-}

-- | Usage: @'glBindFragDataLocationIndexedEXT' program colorNumber index name@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBindFragDataLocationIndexed'.


glBindFragDataLocationIndexedEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> Ptr GLchar -> m ()
glBindFragDataLocationIndexedEXT = ffiuintuintuintPtrcharIOV glBindFragDataLocationIndexedEXTFunPtr

glBindFragDataLocationIndexedEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ())
glBindFragDataLocationIndexedEXTFunPtr = unsafePerformIO (getProcAddress "glBindFragDataLocationIndexedEXT")

{-# NOINLINE glBindFragDataLocationIndexedEXTFunPtr #-}

-- | Usage: @'glGetFragDataIndexEXT' program name@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetFragDataIndex'.


glGetFragDataIndexEXT :: MonadIO m => GLuint -> Ptr GLchar -> m GLint
glGetFragDataIndexEXT = ffiuintPtrcharIOint glGetFragDataIndexEXTFunPtr

glGetFragDataIndexEXTFunPtr :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
glGetFragDataIndexEXTFunPtr = unsafePerformIO (getProcAddress "glGetFragDataIndexEXT")

{-# NOINLINE glGetFragDataIndexEXTFunPtr #-}

-- | Usage: @'glGetProgramResourceLocationIndexEXT' program programInterface name@
--
-- The length of @name@ should be @COMPSIZE(name)@.


glGetProgramResourceLocationIndexEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLchar -> m GLint
glGetProgramResourceLocationIndexEXT = ffiuintenumPtrcharIOint glGetProgramResourceLocationIndexEXTFunPtr

glGetProgramResourceLocationIndexEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
glGetProgramResourceLocationIndexEXTFunPtr = unsafePerformIO (getProcAddress "glGetProgramResourceLocationIndexEXT")

{-# NOINLINE glGetProgramResourceLocationIndexEXTFunPtr #-}

pattern GL_LOCATION_INDEX_EXT  :: (Eq a, Num a) => a

pattern GL_LOCATION_INDEX_EXT = 0x930F

pattern GL_MAX_DUAL_SOURCE_DRAW_BUFFERS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_DUAL_SOURCE_DRAW_BUFFERS_EXT = 0x88FC

pattern GL_ONE_MINUS_SRC1_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_ONE_MINUS_SRC1_ALPHA_EXT = 0x88FB

pattern GL_ONE_MINUS_SRC1_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_ONE_MINUS_SRC1_COLOR_EXT = 0x88FA

pattern GL_SRC1_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_SRC1_ALPHA_EXT = 0x8589

pattern GL_SRC1_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_SRC1_COLOR_EXT = 0x88F9

pattern GL_SRC_ALPHA_SATURATE_EXT  :: (Eq a, Num a) => a

pattern GL_SRC_ALPHA_SATURATE_EXT = 0x0308