-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ES3Compatibility (
  -- * Extension Support
    gl_ARB_ES3_compatibility

  -- * GL_ARB_ES3_compatibility
  , pattern GL_ANY_SAMPLES_PASSED_CONSERVATIVE
  , pattern GL_COMPRESSED_R11_EAC
  , pattern GL_COMPRESSED_RG11_EAC
  , pattern GL_COMPRESSED_RGB8_ETC2
  , pattern GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
  , pattern GL_COMPRESSED_RGBA8_ETC2_EAC
  , pattern GL_COMPRESSED_SIGNED_R11_EAC
  , pattern GL_COMPRESSED_SIGNED_RG11_EAC
  , pattern GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
  , pattern GL_COMPRESSED_SRGB8_ETC2
  , pattern GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
  , pattern GL_MAX_ELEMENT_INDEX
  , pattern GL_PRIMITIVE_RESTART_FIXED_INDEX
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/ES3_compatibility.txt GL_ARB_ES3_compatibility> extension is available.

gl_ARB_ES3_compatibility :: Bool
gl_ARB_ES3_compatibility = member "GL_ARB_ES3_compatibility" extensions
{-# NOINLINE gl_ARB_ES3_compatibility #-}