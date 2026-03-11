-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureBorderClamp (
  -- * Extension Support
    gl_EXT_texture_border_clamp

  -- * GL_EXT_texture_border_clamp
  , glGetSamplerParameterIivEXT
  , glGetSamplerParameterIuivEXT
  , glGetTexParameterIivEXT
  , glGetTexParameterIuivEXT
  , glSamplerParameterIivEXT
  , glSamplerParameterIuivEXT
  , glTexParameterIivEXT
  , glTexParameterIuivEXT
  , pattern GL_CLAMP_TO_BORDER_EXT
  , pattern GL_TEXTURE_BORDER_COLOR_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_border_clamp.txt GL_EXT_texture_border_clamp> extension is available.

gl_EXT_texture_border_clamp :: Bool
gl_EXT_texture_border_clamp = member "GL_EXT_texture_border_clamp" extensions
{-# NOINLINE gl_EXT_texture_border_clamp #-}

-- | Usage: @'glGetSamplerParameterIivEXT' sampler pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetSamplerParameterIiv'.


glGetSamplerParameterIivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetSamplerParameterIivEXT = ffiuintenumPtrintIOV glGetSamplerParameterIivEXTFunPtr

glGetSamplerParameterIivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetSamplerParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glGetSamplerParameterIivEXT")

{-# NOINLINE glGetSamplerParameterIivEXTFunPtr #-}

-- | Usage: @'glGetSamplerParameterIuivEXT' sampler pname params@
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glGetSamplerParameterIuiv'.


glGetSamplerParameterIuivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glGetSamplerParameterIuivEXT = ffiuintenumPtruintIOV glGetSamplerParameterIuivEXTFunPtr

glGetSamplerParameterIuivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glGetSamplerParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glGetSamplerParameterIuivEXT")

{-# NOINLINE glGetSamplerParameterIuivEXTFunPtr #-}

-- | Usage: @'glSamplerParameterIivEXT' sampler pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSamplerParameterIiv'.


glSamplerParameterIivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glSamplerParameterIivEXT = ffiuintenumPtrintIOV glSamplerParameterIivEXTFunPtr

glSamplerParameterIivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glSamplerParameterIivEXTFunPtr = unsafePerformIO (getProcAddress "glSamplerParameterIivEXT")

{-# NOINLINE glSamplerParameterIivEXTFunPtr #-}

-- | Usage: @'glSamplerParameterIuivEXT' sampler pname param@
--
-- The length of @param@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSamplerParameterIuiv'.


glSamplerParameterIuivEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLuint -> m ()
glSamplerParameterIuivEXT = ffiuintenumPtruintIOV glSamplerParameterIuivEXTFunPtr

glSamplerParameterIuivEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
glSamplerParameterIuivEXTFunPtr = unsafePerformIO (getProcAddress "glSamplerParameterIuivEXT")

{-# NOINLINE glSamplerParameterIuivEXTFunPtr #-}

pattern GL_CLAMP_TO_BORDER_EXT  :: (Eq a, Num a) => a

pattern GL_CLAMP_TO_BORDER_EXT = 0x812D

pattern GL_TEXTURE_BORDER_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BORDER_COLOR_EXT = 0x1004