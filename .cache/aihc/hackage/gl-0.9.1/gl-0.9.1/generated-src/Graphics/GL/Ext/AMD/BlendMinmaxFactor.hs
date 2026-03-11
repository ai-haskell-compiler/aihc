-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.BlendMinmaxFactor (
  -- * Extension Support
    gl_AMD_blend_minmax_factor

  -- * GL_AMD_blend_minmax_factor
  , pattern GL_FACTOR_MAX_AMD
  , pattern GL_FACTOR_MIN_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/blend_minmax_factor.txt GL_AMD_blend_minmax_factor> extension is available.

gl_AMD_blend_minmax_factor :: Bool
gl_AMD_blend_minmax_factor = member "GL_AMD_blend_minmax_factor" extensions
{-# NOINLINE gl_AMD_blend_minmax_factor #-}

pattern GL_FACTOR_MAX_AMD  :: (Eq a, Num a) => a

pattern GL_FACTOR_MAX_AMD = 0x901D

pattern GL_FACTOR_MIN_AMD  :: (Eq a, Num a) => a

pattern GL_FACTOR_MIN_AMD = 0x901C