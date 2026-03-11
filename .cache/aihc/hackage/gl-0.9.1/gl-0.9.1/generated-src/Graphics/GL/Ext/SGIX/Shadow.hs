-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Shadow (
  -- * Extension Support
    gl_SGIX_shadow

  -- * GL_SGIX_shadow
  , pattern GL_TEXTURE_COMPARE_OPERATOR_SGIX
  , pattern GL_TEXTURE_COMPARE_SGIX
  , pattern GL_TEXTURE_GEQUAL_R_SGIX
  , pattern GL_TEXTURE_LEQUAL_R_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/shadow.txt GL_SGIX_shadow> extension is available.

gl_SGIX_shadow :: Bool
gl_SGIX_shadow = member "GL_SGIX_shadow" extensions
{-# NOINLINE gl_SGIX_shadow #-}

pattern GL_TEXTURE_COMPARE_OPERATOR_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_OPERATOR_SGIX = 0x819B

pattern GL_TEXTURE_COMPARE_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COMPARE_SGIX = 0x819A

pattern GL_TEXTURE_GEQUAL_R_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_GEQUAL_R_SGIX = 0x819D

pattern GL_TEXTURE_LEQUAL_R_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LEQUAL_R_SGIX = 0x819C