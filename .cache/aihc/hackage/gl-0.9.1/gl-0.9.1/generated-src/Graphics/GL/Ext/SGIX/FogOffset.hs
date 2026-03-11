-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.FogOffset (
  -- * Extension Support
    gl_SGIX_fog_offset

  -- * GL_SGIX_fog_offset
  , pattern GL_FOG_OFFSET_SGIX
  , pattern GL_FOG_OFFSET_VALUE_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/fog_offset.txt GL_SGIX_fog_offset> extension is available.

gl_SGIX_fog_offset :: Bool
gl_SGIX_fog_offset = member "GL_SGIX_fog_offset" extensions
{-# NOINLINE gl_SGIX_fog_offset #-}

pattern GL_FOG_OFFSET_SGIX  :: (Eq a, Num a) => a

pattern GL_FOG_OFFSET_SGIX = 0x8198

pattern GL_FOG_OFFSET_VALUE_SGIX  :: (Eq a, Num a) => a

pattern GL_FOG_OFFSET_VALUE_SGIX = 0x8199