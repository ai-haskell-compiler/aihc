-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.PixelTexture (
  -- * Extension Support
    gl_SGIX_pixel_texture

  -- * GL_SGIX_pixel_texture
  , glPixelTexGenSGIX
  , pattern GL_PIXEL_TEX_GEN_MODE_SGIX
  , pattern GL_PIXEL_TEX_GEN_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_SGIX_pixel_texture extension is available.

gl_SGIX_pixel_texture :: Bool
gl_SGIX_pixel_texture = member "GL_SGIX_pixel_texture" extensions
{-# NOINLINE gl_SGIX_pixel_texture #-}

-- | Usage: @'glPixelTexGenSGIX' mode@
--
-- The parameter @mode@ is a @PixelTexGenModeSGIX@.


glPixelTexGenSGIX :: MonadIO m => GLenum -> m ()
glPixelTexGenSGIX = ffienumIOV glPixelTexGenSGIXFunPtr

glPixelTexGenSGIXFunPtr :: FunPtr (GLenum -> IO ())
glPixelTexGenSGIXFunPtr = unsafePerformIO (getProcAddress "glPixelTexGenSGIX")

{-# NOINLINE glPixelTexGenSGIXFunPtr #-}

pattern GL_PIXEL_TEX_GEN_MODE_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TEX_GEN_MODE_SGIX = 0x832B

pattern GL_PIXEL_TEX_GEN_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_TEX_GEN_SGIX = 0x8139