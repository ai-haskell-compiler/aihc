-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IBM.CullVertex (
  -- * Extension Support
    gl_IBM_cull_vertex

  -- * GL_IBM_cull_vertex
  , pattern GL_CULL_VERTEX_IBM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/IBM/cull_vertex.txt GL_IBM_cull_vertex> extension is available.

gl_IBM_cull_vertex :: Bool
gl_IBM_cull_vertex = member "GL_IBM_cull_vertex" extensions
{-# NOINLINE gl_IBM_cull_vertex #-}

pattern GL_CULL_VERTEX_IBM  :: (Eq a, Num a) => a

pattern GL_CULL_VERTEX_IBM = 103050