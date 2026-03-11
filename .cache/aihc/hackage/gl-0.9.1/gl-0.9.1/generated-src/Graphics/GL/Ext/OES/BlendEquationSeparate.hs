-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.BlendEquationSeparate (
  -- * Extension Support
    gl_OES_blend_equation_separate

  -- * GL_OES_blend_equation_separate
  , glBlendEquationSeparateOES
  , pattern GL_BLEND_EQUATION_ALPHA_OES
  , pattern GL_BLEND_EQUATION_RGB_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_blend_equation_separate.txt GL_OES_blend_equation_separate> extension is available.

gl_OES_blend_equation_separate :: Bool
gl_OES_blend_equation_separate = member "GL_OES_blend_equation_separate" extensions
{-# NOINLINE gl_OES_blend_equation_separate #-}

-- | Usage: @'glBlendEquationSeparateOES' modeRGB modeAlpha@


glBlendEquationSeparateOES :: MonadIO m => GLenum -> GLenum -> m ()
glBlendEquationSeparateOES = ffienumenumIOV glBlendEquationSeparateOESFunPtr

glBlendEquationSeparateOESFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glBlendEquationSeparateOESFunPtr = unsafePerformIO (getProcAddress "glBlendEquationSeparateOES")

{-# NOINLINE glBlendEquationSeparateOESFunPtr #-}

pattern GL_BLEND_EQUATION_ALPHA_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_EQUATION_ALPHA_OES = 0x883D

pattern GL_BLEND_EQUATION_RGB_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_EQUATION_RGB_OES = 0x8009