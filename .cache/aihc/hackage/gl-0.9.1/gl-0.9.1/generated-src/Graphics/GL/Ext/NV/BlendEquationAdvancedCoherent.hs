-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.BlendEquationAdvancedCoherent (
  -- * Extension Support
    gl_NV_blend_equation_advanced_coherent

  -- * GL_NV_blend_equation_advanced_coherent
  , pattern GL_BLEND_ADVANCED_COHERENT_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_blend_equation_advanced_coherent.txt GL_NV_blend_equation_advanced_coherent> extension is available.

gl_NV_blend_equation_advanced_coherent :: Bool
gl_NV_blend_equation_advanced_coherent = member "GL_NV_blend_equation_advanced_coherent" extensions
{-# NOINLINE gl_NV_blend_equation_advanced_coherent #-}

pattern GL_BLEND_ADVANCED_COHERENT_NV  :: (Eq a, Num a) => a

pattern GL_BLEND_ADVANCED_COHERENT_NV = 0x9285