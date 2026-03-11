-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.TextureView (
  -- * Extension Support
    gl_OES_texture_view

  -- * GL_OES_texture_view
  , glTextureViewOES
  , pattern GL_TEXTURE_IMMUTABLE_LEVELS
  , pattern GL_TEXTURE_VIEW_MIN_LAYER_OES
  , pattern GL_TEXTURE_VIEW_MIN_LEVEL_OES
  , pattern GL_TEXTURE_VIEW_NUM_LAYERS_OES
  , pattern GL_TEXTURE_VIEW_NUM_LEVELS_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_texture_view.txt GL_OES_texture_view> extension is available.

gl_OES_texture_view :: Bool
gl_OES_texture_view = member "GL_OES_texture_view" extensions
{-# NOINLINE gl_OES_texture_view #-}

-- | Usage: @'glTextureViewOES' texture target origtexture internalformat minlevel numlevels minlayer numlayers@
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glTextureView'.


glTextureViewOES :: MonadIO m => GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glTextureViewOES = ffiuintenumuintenumuintuintuintuintIOV glTextureViewOESFunPtr

glTextureViewOESFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glTextureViewOESFunPtr = unsafePerformIO (getProcAddress "glTextureViewOES")

{-# NOINLINE glTextureViewOESFunPtr #-}

pattern GL_TEXTURE_VIEW_MIN_LAYER_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_MIN_LAYER_OES = 0x82DD

pattern GL_TEXTURE_VIEW_MIN_LEVEL_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_MIN_LEVEL_OES = 0x82DB

pattern GL_TEXTURE_VIEW_NUM_LAYERS_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_NUM_LAYERS_OES = 0x82DE

pattern GL_TEXTURE_VIEW_NUM_LEVELS_OES  :: (Eq a, Num a) => a

pattern GL_TEXTURE_VIEW_NUM_LEVELS_OES = 0x82DC