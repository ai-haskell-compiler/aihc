-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.FramebufferDownsample (
  -- * Extension Support
    gl_IMG_framebuffer_downsample

  -- * GL_IMG_framebuffer_downsample
  , glFramebufferTexture2DDownsampleIMG
  , glFramebufferTextureLayerDownsampleIMG
  , pattern GL_DOWNSAMPLE_SCALES_IMG
  , pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_SCALE_IMG
  , pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_AND_DOWNSAMPLE_IMG
  , pattern GL_NUM_DOWNSAMPLE_SCALES_IMG
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/IMG/IMG_framebuffer_downsample.txt GL_IMG_framebuffer_downsample> extension is available.

gl_IMG_framebuffer_downsample :: Bool
gl_IMG_framebuffer_downsample = member "GL_IMG_framebuffer_downsample" extensions
{-# NOINLINE gl_IMG_framebuffer_downsample #-}

-- | Usage: @'glFramebufferTexture2DDownsampleIMG' target attachment textarget texture level xscale yscale@


glFramebufferTexture2DDownsampleIMG :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> m ()
glFramebufferTexture2DDownsampleIMG = ffienumenumenumuintintintintIOV glFramebufferTexture2DDownsampleIMGFunPtr

glFramebufferTexture2DDownsampleIMGFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> IO ())
glFramebufferTexture2DDownsampleIMGFunPtr = unsafePerformIO (getProcAddress "glFramebufferTexture2DDownsampleIMG")

{-# NOINLINE glFramebufferTexture2DDownsampleIMGFunPtr #-}

-- | Usage: @'glFramebufferTextureLayerDownsampleIMG' target attachment texture level layer xscale yscale@
--
-- The parameter @target@ is a @FramebufferTarget@.
--
-- The parameter @attachment@ is a @FramebufferAttachment@.
--
-- The parameter @texture@ is a @Texture@.
--
-- The parameter @level@ is a @CheckedInt32@.
--
-- The parameter @layer@ is a @CheckedInt32@.


glFramebufferTextureLayerDownsampleIMG :: MonadIO m => GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> m ()
glFramebufferTextureLayerDownsampleIMG = ffienumenumuintintintintintIOV glFramebufferTextureLayerDownsampleIMGFunPtr

glFramebufferTextureLayerDownsampleIMGFunPtr :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
glFramebufferTextureLayerDownsampleIMGFunPtr = unsafePerformIO (getProcAddress "glFramebufferTextureLayerDownsampleIMG")

{-# NOINLINE glFramebufferTextureLayerDownsampleIMGFunPtr #-}

pattern GL_DOWNSAMPLE_SCALES_IMG  :: (Eq a, Num a) => a

pattern GL_DOWNSAMPLE_SCALES_IMG = 0x913E

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_SCALE_IMG  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_SCALE_IMG = 0x913F

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_AND_DOWNSAMPLE_IMG  :: (Eq a, Num a) => a

pattern GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE_AND_DOWNSAMPLE_IMG = 0x913C

pattern GL_NUM_DOWNSAMPLE_SCALES_IMG  :: (Eq a, Num a) => a

pattern GL_NUM_DOWNSAMPLE_SCALES_IMG = 0x913D