-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.PGI.VertexHints (
  -- * Extension Support
    gl_PGI_vertex_hints

  -- * GL_PGI_vertex_hints
  , pattern GL_COLOR3_BIT_PGI
  , pattern GL_COLOR4_BIT_PGI
  , pattern GL_EDGEFLAG_BIT_PGI
  , pattern GL_INDEX_BIT_PGI
  , pattern GL_MATERIAL_SIDE_HINT_PGI
  , pattern GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI
  , pattern GL_MAT_AMBIENT_BIT_PGI
  , pattern GL_MAT_COLOR_INDEXES_BIT_PGI
  , pattern GL_MAT_DIFFUSE_BIT_PGI
  , pattern GL_MAT_EMISSION_BIT_PGI
  , pattern GL_MAT_SHININESS_BIT_PGI
  , pattern GL_MAT_SPECULAR_BIT_PGI
  , pattern GL_MAX_VERTEX_HINT_PGI
  , pattern GL_NORMAL_BIT_PGI
  , pattern GL_TEXCOORD1_BIT_PGI
  , pattern GL_TEXCOORD2_BIT_PGI
  , pattern GL_TEXCOORD3_BIT_PGI
  , pattern GL_TEXCOORD4_BIT_PGI
  , pattern GL_VERTEX23_BIT_PGI
  , pattern GL_VERTEX4_BIT_PGI
  , pattern GL_VERTEX_CONSISTENT_HINT_PGI
  , pattern GL_VERTEX_DATA_HINT_PGI
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/PGI/vertex_hints.txt GL_PGI_vertex_hints> extension is available.

gl_PGI_vertex_hints :: Bool
gl_PGI_vertex_hints = member "GL_PGI_vertex_hints" extensions
{-# NOINLINE gl_PGI_vertex_hints #-}

pattern GL_COLOR3_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_COLOR3_BIT_PGI = 0x00010000

pattern GL_COLOR4_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_COLOR4_BIT_PGI = 0x00020000

pattern GL_EDGEFLAG_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_EDGEFLAG_BIT_PGI = 0x00040000

pattern GL_INDEX_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_INDEX_BIT_PGI = 0x00080000

pattern GL_MATERIAL_SIDE_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_MATERIAL_SIDE_HINT_PGI = 0x1A22C

pattern GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI = 0x00200000

pattern GL_MAT_AMBIENT_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_AMBIENT_BIT_PGI = 0x00100000

pattern GL_MAT_COLOR_INDEXES_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_COLOR_INDEXES_BIT_PGI = 0x01000000

pattern GL_MAT_DIFFUSE_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_DIFFUSE_BIT_PGI = 0x00400000

pattern GL_MAT_EMISSION_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_EMISSION_BIT_PGI = 0x00800000

pattern GL_MAT_SHININESS_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_SHININESS_BIT_PGI = 0x02000000

pattern GL_MAT_SPECULAR_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_MAT_SPECULAR_BIT_PGI = 0x04000000

pattern GL_MAX_VERTEX_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_HINT_PGI = 0x1A22D

pattern GL_NORMAL_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_NORMAL_BIT_PGI = 0x08000000

pattern GL_TEXCOORD1_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_TEXCOORD1_BIT_PGI = 0x10000000

pattern GL_TEXCOORD2_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_TEXCOORD2_BIT_PGI = 0x20000000

pattern GL_TEXCOORD3_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_TEXCOORD3_BIT_PGI = 0x40000000

pattern GL_TEXCOORD4_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_TEXCOORD4_BIT_PGI = 0x80000000

pattern GL_VERTEX23_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_VERTEX23_BIT_PGI = 0x00000004

pattern GL_VERTEX4_BIT_PGI  :: (Eq a, Num a) => a

pattern GL_VERTEX4_BIT_PGI = 0x00000008

pattern GL_VERTEX_CONSISTENT_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_VERTEX_CONSISTENT_HINT_PGI = 0x1A22B

pattern GL_VERTEX_DATA_HINT_PGI  :: (Eq a, Num a) => a

pattern GL_VERTEX_DATA_HINT_PGI = 0x1A22A