-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.SpecularVector (
  -- * Extension Support
    gl_APPLE_specular_vector

  -- * GL_APPLE_specular_vector
  , pattern GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/specular_vector.txt GL_APPLE_specular_vector> extension is available.

gl_APPLE_specular_vector :: Bool
gl_APPLE_specular_vector = member "GL_APPLE_specular_vector" extensions
{-# NOINLINE gl_APPLE_specular_vector #-}

pattern GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE  :: (Eq a, Num a) => a

pattern GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE = 0x85B0