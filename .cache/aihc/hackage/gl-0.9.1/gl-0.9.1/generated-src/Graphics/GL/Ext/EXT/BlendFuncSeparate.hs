-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendFuncSeparate (
  -- * Extension Support
    gl_EXT_blend_func_separate

  -- * GL_EXT_blend_func_separate
  , glBlendFuncSeparateEXT
  , pattern GL_BLEND_DST_ALPHA_EXT
  , pattern GL_BLEND_DST_RGB_EXT
  , pattern GL_BLEND_SRC_ALPHA_EXT
  , pattern GL_BLEND_SRC_RGB_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/blend_func_separate.txt GL_EXT_blend_func_separate> extension is available.

gl_EXT_blend_func_separate :: Bool
gl_EXT_blend_func_separate = member "GL_EXT_blend_func_separate" extensions
{-# NOINLINE gl_EXT_blend_func_separate #-}

-- | Usage: @'glBlendFuncSeparateEXT' sfactorRGB dfactorRGB sfactorAlpha dfactorAlpha@
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


glBlendFuncSeparateEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> m ()
glBlendFuncSeparateEXT = ffienumenumenumenumIOV glBlendFuncSeparateEXTFunPtr

glBlendFuncSeparateEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glBlendFuncSeparateEXTFunPtr = unsafePerformIO (getProcAddress "glBlendFuncSeparateEXT")

{-# NOINLINE glBlendFuncSeparateEXTFunPtr #-}

pattern GL_BLEND_DST_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_DST_ALPHA_EXT = 0x80CA

pattern GL_BLEND_DST_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_DST_RGB_EXT = 0x80C8

pattern GL_BLEND_SRC_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_SRC_ALPHA_EXT = 0x80CB

pattern GL_BLEND_SRC_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_SRC_RGB_EXT = 0x80C9