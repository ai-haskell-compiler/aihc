-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.BlendEquationAdvancedCoherent (
  -- * Extension Support
    gl_KHR_blend_equation_advanced_coherent

  -- * GL_KHR_blend_equation_advanced_coherent
  , pattern GL_BLEND_ADVANCED_COHERENT_KHR
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/KHR/KHR_blend_equation_advanced_coherent.txt GL_KHR_blend_equation_advanced_coherent> extension is available.

gl_KHR_blend_equation_advanced_coherent :: Bool
gl_KHR_blend_equation_advanced_coherent = member "GL_KHR_blend_equation_advanced_coherent" extensions
{-# NOINLINE gl_KHR_blend_equation_advanced_coherent #-}

pattern GL_BLEND_ADVANCED_COHERENT_KHR  :: (Eq a, Num a) => a

pattern GL_BLEND_ADVANCED_COHERENT_KHR = 0x9285