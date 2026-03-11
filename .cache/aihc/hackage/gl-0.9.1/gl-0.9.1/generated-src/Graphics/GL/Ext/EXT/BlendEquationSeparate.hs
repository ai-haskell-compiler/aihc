-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendEquationSeparate (
  -- * Extension Support
    gl_EXT_blend_equation_separate

  -- * GL_EXT_blend_equation_separate
  , glBlendEquationSeparateEXT
  , pattern GL_BLEND_EQUATION_ALPHA_EXT
  , pattern GL_BLEND_EQUATION_RGB_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/blend_equation_separate.txt GL_EXT_blend_equation_separate> extension is available.

gl_EXT_blend_equation_separate :: Bool
gl_EXT_blend_equation_separate = member "GL_EXT_blend_equation_separate" extensions
{-# NOINLINE gl_EXT_blend_equation_separate #-}

-- | Usage: @'glBlendEquationSeparateEXT' modeRGB modeAlpha@
--
-- The parameter @modeRGB@ is a @BlendEquationModeEXT@, one of: 'Graphics.GL.Ext.SGIX.BlendAlphaMinmax.GL_ALPHA_MAX_SGIX', 'Graphics.GL.Ext.SGIX.BlendAlphaMinmax.GL_ALPHA_MIN_SGIX', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_FUNC_ADD_EXT', 'Graphics.GL.Ext.EXT.BlendSubtract.GL_FUNC_REVERSE_SUBTRACT_EXT', 'Graphics.GL.Ext.EXT.BlendSubtract.GL_FUNC_SUBTRACT_EXT', 'Graphics.GL.Internal.Shared.GL_LOGIC_OP', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_MAX_EXT', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_MIN_EXT'.
--
-- The parameter @modeAlpha@ is a @BlendEquationModeEXT@, one of: 'Graphics.GL.Ext.SGIX.BlendAlphaMinmax.GL_ALPHA_MAX_SGIX', 'Graphics.GL.Ext.SGIX.BlendAlphaMinmax.GL_ALPHA_MIN_SGIX', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_FUNC_ADD_EXT', 'Graphics.GL.Ext.EXT.BlendSubtract.GL_FUNC_REVERSE_SUBTRACT_EXT', 'Graphics.GL.Ext.EXT.BlendSubtract.GL_FUNC_SUBTRACT_EXT', 'Graphics.GL.Internal.Shared.GL_LOGIC_OP', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_MAX_EXT', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_MIN_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquationSeparate'.


glBlendEquationSeparateEXT :: MonadIO m => GLenum -> GLenum -> m ()
glBlendEquationSeparateEXT = ffienumenumIOV glBlendEquationSeparateEXTFunPtr

glBlendEquationSeparateEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glBlendEquationSeparateEXTFunPtr = unsafePerformIO (getProcAddress "glBlendEquationSeparateEXT")

{-# NOINLINE glBlendEquationSeparateEXTFunPtr #-}

pattern GL_BLEND_EQUATION_ALPHA_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_EQUATION_ALPHA_EXT = 0x883D

pattern GL_BLEND_EQUATION_RGB_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_EQUATION_RGB_EXT = 0x8009