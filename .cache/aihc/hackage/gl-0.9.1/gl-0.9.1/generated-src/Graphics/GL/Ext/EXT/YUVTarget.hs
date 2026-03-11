-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.YUVTarget (
  -- * Extension Support
    gl_EXT_YUV_target

  -- * GL_EXT_YUV_target
  , pattern GL_REQUIRED_TEXTURE_IMAGE_UNITS_OES
  , pattern GL_SAMPLER_EXTERNAL_2D_Y2Y_EXT
  , pattern GL_TEXTURE_BINDING_EXTERNAL_OES
  , pattern GL_TEXTURE_EXTERNAL_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_YUV_target.txt GL_EXT_YUV_target> extension is available.

gl_EXT_YUV_target :: Bool
gl_EXT_YUV_target = member "GL_EXT_YUV_target" extensions
{-# NOINLINE gl_EXT_YUV_target #-}

pattern GL_SAMPLER_EXTERNAL_2D_Y2Y_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLER_EXTERNAL_2D_Y2Y_EXT = 0x8BE7