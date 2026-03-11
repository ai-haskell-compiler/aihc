-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.BlendSubtract (
  -- * Extension Support
    gl_OES_blend_subtract

  -- * GL_OES_blend_subtract
  , glBlendEquationOES
  , pattern GL_BLEND_EQUATION_OES
  , pattern GL_FUNC_ADD_OES
  , pattern GL_FUNC_REVERSE_SUBTRACT_OES
  , pattern GL_FUNC_SUBTRACT_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_blend_subtract.txt GL_OES_blend_subtract> extension is available.

gl_OES_blend_subtract :: Bool
gl_OES_blend_subtract = member "GL_OES_blend_subtract" extensions
{-# NOINLINE gl_OES_blend_subtract #-}

-- | Usage: @'glBlendEquationOES' mode@


glBlendEquationOES :: MonadIO m => GLenum -> m ()
glBlendEquationOES = ffienumIOV glBlendEquationOESFunPtr

glBlendEquationOESFunPtr :: FunPtr (GLenum -> IO ())
glBlendEquationOESFunPtr = unsafePerformIO (getProcAddress "glBlendEquationOES")

{-# NOINLINE glBlendEquationOESFunPtr #-}

pattern GL_BLEND_EQUATION_OES  :: (Eq a, Num a) => a

pattern GL_BLEND_EQUATION_OES = 0x8009

pattern GL_FUNC_ADD_OES  :: (Eq a, Num a) => a

pattern GL_FUNC_ADD_OES = 0x8006

pattern GL_FUNC_REVERSE_SUBTRACT_OES  :: (Eq a, Num a) => a

pattern GL_FUNC_REVERSE_SUBTRACT_OES = 0x800B

pattern GL_FUNC_SUBTRACT_OES  :: (Eq a, Num a) => a

pattern GL_FUNC_SUBTRACT_OES = 0x800A