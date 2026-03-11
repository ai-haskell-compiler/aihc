-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MultisampleCompatibility (
  -- * Extension Support
    gl_EXT_multisample_compatibility

  -- * GL_EXT_multisample_compatibility
  , pattern GL_MULTISAMPLE_EXT
  , pattern GL_SAMPLE_ALPHA_TO_ONE_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_multisample_compatibility.txt GL_EXT_multisample_compatibility> extension is available.

gl_EXT_multisample_compatibility :: Bool
gl_EXT_multisample_compatibility = member "GL_EXT_multisample_compatibility" extensions
{-# NOINLINE gl_EXT_multisample_compatibility #-}