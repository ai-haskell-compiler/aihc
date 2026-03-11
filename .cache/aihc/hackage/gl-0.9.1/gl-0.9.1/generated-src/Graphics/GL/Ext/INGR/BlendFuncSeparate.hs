-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.INGR.BlendFuncSeparate (
  -- * Extension Support
    gl_INGR_blend_func_separate

  -- * GL_INGR_blend_func_separate
  , glBlendFuncSeparateINGR
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_INGR_blend_func_separate extension is available.

gl_INGR_blend_func_separate :: Bool
gl_INGR_blend_func_separate = member "GL_INGR_blend_func_separate" extensions
{-# NOINLINE gl_INGR_blend_func_separate #-}

-- | Usage: @'glBlendFuncSeparateINGR' sfactorRGB dfactorRGB sfactorAlpha dfactorAlpha@
--
-- The parameter @sfactorRGB@ is a @BlendFuncSeparateParameterEXT@.
--
-- The parameter @dfactorRGB@ is a @BlendFuncSeparateParameterEXT@.
--
-- The parameter @sfactorAlpha@ is a @BlendFuncSeparateParameterEXT@.
--
-- The parameter @dfactorAlpha@ is a @BlendFuncSeparateParameterEXT@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendFuncSeparate'.


glBlendFuncSeparateINGR :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateINGR = ffienumenumenumenumIOV glBlendFuncSeparateINGRFunPtr

glBlendFuncSeparateINGRFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateINGRFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateINGR")

{-# NOINLINE glBlendFuncSeparateINGRFunPtr #-}