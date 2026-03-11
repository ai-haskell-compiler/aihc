-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.BlendFuncSeparate (
  -- * Extension Support
    gl_OES_blend_func_separate

  -- * GL_OES_blend_func_separate
  , glBlendFuncSeparateOES
  , pattern GL_BLEND_DST_ALPHA_OES
  , pattern GL_BLEND_DST_RGB_OES
  , pattern GL_BLEND_SRC_ALPHA_OES
  , pattern GL_BLEND_SRC_RGB_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_blend_func_separate.txt GL_OES_blend_func_separate> extension is available.

gl_OES_blend_func_separate :: Bool
gl_OES_blend_func_separate = member "GL_OES_blend_func_separate" extensions
{-# NOINLINE gl_OES_blend_func_separate #-}

-- | Usage: @'glBlendFuncSeparateOES' srcRGB dstRGB srcAlpha dstAlpha@


glBlendFuncSeparateOES :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateOES = ffienumenumenumenumIOV glBlendFuncSeparateOESFunPtr

glBlendFuncSeparateOESFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateOESFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateOES")

{-# NOINLINE glBlendFuncSeparateOESFunPtr #-}

pattern GL_BLEND_DST_ALPHA_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_DST_ALPHA_OES = 0x80CA

pattern GL_BLEND_DST_RGB_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_DST_RGB_OES = 0x80C8

pattern GL_BLEND_SRC_ALPHA_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_SRC_ALPHA_OES = 0x80CB

pattern GL_BLEND_SRC_RGB_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_SRC_RGB_OES = 0x80C9