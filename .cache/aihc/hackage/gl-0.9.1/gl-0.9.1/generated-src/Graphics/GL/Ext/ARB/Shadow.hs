-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.Shadow (
  -- * Extension Support
    gl_ARB_shadow

  -- * GL_ARB_shadow
  , pattern GL_COMPARE_R_TO_TEXTURE_ARB
  , pattern GL_TEXTURE_COMPARE_FUNC_ARB
  , pattern GL_TEXTURE_COMPARE_MODE_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shadow.txt GL_ARB_shadow> extension is available.

gl_ARB_shadow :: Bool
gl_ARB_shadow = member "GL_ARB_shadow" extensions
{-# NOINLINE gl_ARB_shadow #-}

pattern GL_COMPARE_R_TO_TEXTURE_ARB  :: (Eq a, Num a) => a

pattern GL_COMPARE_R_TO_TEXTURE_ARB = 0x884E

pattern GL_TEXTURE_COMPARE_FUNC_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_FUNC_ARB = 0x884D

pattern GL_TEXTURE_COMPARE_MODE_ARB  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_MODE_ARB = 0x884C