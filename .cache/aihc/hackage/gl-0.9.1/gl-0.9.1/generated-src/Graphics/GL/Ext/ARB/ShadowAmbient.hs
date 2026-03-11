-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShadowAmbient (
  -- * Extension Support
    gl_ARB_shadow_ambient

  -- * GL_ARB_shadow_ambient
  , pattern GL_TEXTURE_COMPARE_FAIL_VALUE_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shadow_ambient.txt GL_ARB_shadow_ambient> extension is available.

gl_ARB_shadow_ambient :: Bool
gl_ARB_shadow_ambient = member "GL_ARB_shadow_ambient" extensions
{-# NOINLINE gl_ARB_shadow_ambient #-}

pattern GL_TEXTURE_COMPARE_FAIL_VALUE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_FAIL_VALUE_ARB = 0x80BF