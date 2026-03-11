-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ShadingLanguage100 (
  -- * Extension Support
    gl_ARB_shading_language_100

  -- * GL_ARB_shading_language_100
  , pattern GL_SHADING_LANGUAGE_VERSION_ARB
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/shading_language_100.txt GL_ARB_shading_language_100> extension is available.

gl_ARB_shading_language_100 :: Bool
gl_ARB_shading_language_100 = member "GL_ARB_shading_language_100" extensions
{-# NOINLINE gl_ARB_shading_language_100 #-}

pattern GL_SHADING_LANGUAGE_VERSION_ARB  :: (Eq a, Num a) => a

pattern GL_SHADING_LANGUAGE_VERSION_ARB = 0x8B8C