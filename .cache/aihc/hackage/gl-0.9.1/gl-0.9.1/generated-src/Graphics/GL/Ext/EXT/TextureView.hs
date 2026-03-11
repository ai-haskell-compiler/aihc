-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.TextureView (
  -- * Extension Support
    gl_EXT_texture_view

  -- * GL_EXT_texture_view
  , glTextureViewEXT
  , pattern GL_TEXTURE_IMMUTABLE_LEVELS
  , pattern GL_TEXTURE_VIEW_MIN_LAYER_EXT
  , pattern GL_TEXTURE_VIEW_MIN_LEVEL_EXT
  , pattern GL_TEXTURE_VIEW_NUM_LAYERS_EXT
  , pattern GL_TEXTURE_VIEW_NUM_LEVELS_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_texture_view.txt GL_EXT_texture_view> extension is available.

gl_EXT_texture_view :: Bool
gl_EXT_texture_view = member "GL_EXT_texture_view" extensions
{-# NOINLINE gl_EXT_texture_view #-}

-- | Usage: @'glTextureViewEXT' texture target origtexture internalformat minlevel numlevels minlayer numlayers@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTextureView'.


glTextureViewEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glTextureViewEXT = ffiuintenumuintenumuintuintuintuintIOV glTextureViewEXTFunPtr

glTextureViewEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glTextureViewEXTFunPtr = unsafePerformIO (getProcAddress "glTextureViewEXT")

{-# NOINLINE glTextureViewEXTFunPtr #-}

pattern GL_TEXTURE_VIEW_MIN_LAYER_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_MIN_LAYER_EXT = 0x82DD

pattern GL_TEXTURE_VIEW_MIN_LEVEL_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_MIN_LEVEL_EXT = 0x82DB

pattern GL_TEXTURE_VIEW_NUM_LAYERS_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_NUM_LAYERS_EXT = 0x82DE

pattern GL_TEXTURE_VIEW_NUM_LEVELS_EXT  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_NUM_LEVELS_EXT = 0x82DC