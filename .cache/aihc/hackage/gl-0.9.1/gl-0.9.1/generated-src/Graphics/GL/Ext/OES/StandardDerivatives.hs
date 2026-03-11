-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.StandardDerivatives (
  -- * Extension Support
    gl_OES_standard_derivatives

  -- * GL_OES_standard_derivatives
  , pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_standard_derivatives.txt GL_OES_standard_derivatives> extension is available.

gl_OES_standard_derivatives :: Bool
gl_OES_standard_derivatives = member "GL_OES_standard_derivatives" extensions
{-# NOINLINE gl_OES_standard_derivatives #-}

pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_DERIVATIVE_HINT_OES = 0x8B8B