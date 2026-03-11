-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.VertexPreclip (
  -- * Extension Support
    gl_SGIX_vertex_preclip

  -- * GL_SGIX_vertex_preclip
  , pattern GL_VERTEX_PRECLIP_HINT_SGIX
  , pattern GL_VERTEX_PRECLIP_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/vertex_preclip.txt GL_SGIX_vertex_preclip> extension is available.

gl_SGIX_vertex_preclip :: Bool
gl_SGIX_vertex_preclip = member "GL_SGIX_vertex_preclip" extensions
{-# NOINLINE gl_SGIX_vertex_preclip #-}

pattern GL_VERTEX_PRECLIP_HINT_SGIX  :: (Eq a, Num a) => a

pattern GL_VERTEX_PRECLIP_HINT_SGIX = 0x83EF

pattern GL_VERTEX_PRECLIP_SGIX  :: (Eq a, Num a) => a

pattern GL_VERTEX_PRECLIP_SGIX = 0x83EE