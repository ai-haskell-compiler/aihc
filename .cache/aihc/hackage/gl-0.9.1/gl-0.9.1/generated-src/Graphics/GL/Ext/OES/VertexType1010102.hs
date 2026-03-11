-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.VertexType1010102 (
  -- * Extension Support
    gl_OES_vertex_type_10_10_10_2

  -- * GL_OES_vertex_type_10_10_10_2
  , pattern GL_INT_10_10_10_2_OES
  , pattern GL_UNSIGNED_INT_10_10_10_2_OES
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_vertex_type_10_10_10_2.txt GL_OES_vertex_type_10_10_10_2> extension is available.

gl_OES_vertex_type_10_10_10_2 :: Bool
gl_OES_vertex_type_10_10_10_2 = member "GL_OES_vertex_type_10_10_10_2" extensions
{-# NOINLINE gl_OES_vertex_type_10_10_10_2 #-}

pattern GL_INT_10_10_10_2_OES  :: (Eq a, Num a) => a

pattern GL_INT_10_10_10_2_OES = 0x8DF7

pattern GL_UNSIGNED_INT_10_10_10_2_OES  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_10_10_10_2_OES = 0x8DF6