-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.TextureEdgeClamp (
  -- * Extension Support
    gl_SGIS_texture_edge_clamp

  -- * GL_SGIS_texture_edge_clamp
  , pattern GL_CLAMP_TO_EDGE_SGIS
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/texture_edge_clamp.txt GL_SGIS_texture_edge_clamp> extension is available.

gl_SGIS_texture_edge_clamp :: Bool
gl_SGIS_texture_edge_clamp = member "GL_SGIS_texture_edge_clamp" extensions
{-# NOINLINE gl_SGIS_texture_edge_clamp #-}

pattern GL_CLAMP_TO_EDGE_SGIS  :: (Eq a, Num a) => a

pattern GL_CLAMP_TO_EDGE_SGIS = 0x812F