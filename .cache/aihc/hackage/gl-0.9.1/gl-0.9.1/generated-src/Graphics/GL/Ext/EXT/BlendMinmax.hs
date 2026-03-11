-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.BlendMinmax (
  -- * Extension Support
    gl_EXT_blend_minmax

  -- * GL_EXT_blend_minmax
  , glBlendEquationEXT
  , pattern GL_BLEND_EQUATION_EXT
  , pattern GL_FUNC_ADD_EXT
  , pattern GL_MAX_EXT
  , pattern GL_MIN_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_blend_minmax.txt GL_EXT_blend_minmax> extension is available.

gl_EXT_blend_minmax :: Bool
gl_EXT_blend_minmax = member "GL_EXT_blend_minmax" extensions
{-# NOINLINE gl_EXT_blend_minmax #-}

-- | Usage: @'glBlendEquationEXT' mode@
--
-- The parameter @mode@ is a @BlendEquationModeEXT@, one of: 'Graphics.GL.Ext.SGIX.BlendAlphaMinmax.GL_ALPHA_MAX_SGIX', 'Graphics.GL.Ext.SGIX.BlendAlphaMinmax.GL_ALPHA_MIN_SGIX', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_FUNC_ADD_EXT', 'Graphics.GL.Ext.EXT.BlendSubtract.GL_FUNC_REVERSE_SUBTRACT_EXT', 'Graphics.GL.Ext.EXT.BlendSubtract.GL_FUNC_SUBTRACT_EXT', 'Graphics.GL.Internal.Shared.GL_LOGIC_OP', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_MAX_EXT', 'Graphics.GL.Ext.EXT.BlendMinmax.GL_MIN_EXT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glBlendEquation'.


glBlendEquationEXT :: MonadIO m => GLenum -> m ()
glBlendEquationEXT = ffienumIOV glBlendEquationEXTFunPtr

glBlendEquationEXTFunPtr :: FunPtr (GLenum -> IO ())
glBlendEquationEXTFunPtr = unsafePerformIO (getProcAddress "glBlendEquationEXT")

{-# NOINLINE glBlendEquationEXTFunPtr #-}

pattern GL_BLEND_EQUATION_EXT  :: (Eq a, Num a) => a

pattern GL_BLEND_EQUATION_EXT = 0x8009

pattern GL_FUNC_ADD_EXT  :: (Eq a, Num a) => a

pattern GL_FUNC_ADD_EXT = 0x8006

pattern GL_MAX_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_EXT = 0x8008

pattern GL_MIN_EXT  :: (Eq a, Num a) => a

pattern GL_MIN_EXT = 0x8007