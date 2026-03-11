-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Clipmap (
  -- * Extension Support
    gl_SGIX_clipmap

  -- * GL_SGIX_clipmap
  , pattern GL_LINEAR_CLIPMAP_LINEAR_SGIX
  , pattern GL_LINEAR_CLIPMAP_NEAREST_SGIX
  , pattern GL_MAX_CLIPMAP_DEPTH_SGIX
  , pattern GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX
  , pattern GL_NEAREST_CLIPMAP_LINEAR_SGIX
  , pattern GL_NEAREST_CLIPMAP_NEAREST_SGIX
  , pattern GL_TEXTURE_CLIPMAP_CENTER_SGIX
  , pattern GL_TEXTURE_CLIPMAP_DEPTH_SGIX
  , pattern GL_TEXTURE_CLIPMAP_FRAME_SGIX
  , pattern GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX
  , pattern GL_TEXTURE_CLIPMAP_OFFSET_SGIX
  , pattern GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/clipmap.txt GL_SGIX_clipmap> extension is available.

gl_SGIX_clipmap :: Bool
gl_SGIX_clipmap = member "GL_SGIX_clipmap" extensions
{-# NOINLINE gl_SGIX_clipmap #-}

pattern GL_LINEAR_CLIPMAP_LINEAR_SGIX  :: (Eq a, Num a) => a

pattern GL_LINEAR_CLIPMAP_LINEAR_SGIX = 0x8170

pattern GL_LINEAR_CLIPMAP_NEAREST_SGIX  :: (Eq a, Num a) => a

pattern GL_LINEAR_CLIPMAP_NEAREST_SGIX = 0x844F

pattern GL_MAX_CLIPMAP_DEPTH_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_CLIPMAP_DEPTH_SGIX = 0x8177

pattern GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX  :: (Eq a, Num a) => a

pattern GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX = 0x8178

pattern GL_NEAREST_CLIPMAP_LINEAR_SGIX  :: (Eq a, Num a) => a

pattern GL_NEAREST_CLIPMAP_LINEAR_SGIX = 0x844E

pattern GL_NEAREST_CLIPMAP_NEAREST_SGIX  :: (Eq a, Num a) => a

pattern GL_NEAREST_CLIPMAP_NEAREST_SGIX = 0x844D

pattern GL_TEXTURE_CLIPMAP_CENTER_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CLIPMAP_CENTER_SGIX = 0x8171

pattern GL_TEXTURE_CLIPMAP_DEPTH_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CLIPMAP_DEPTH_SGIX = 0x8176

pattern GL_TEXTURE_CLIPMAP_FRAME_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CLIPMAP_FRAME_SGIX = 0x8172

pattern GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX = 0x8175

pattern GL_TEXTURE_CLIPMAP_OFFSET_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CLIPMAP_OFFSET_SGIX = 0x8173

pattern GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX  :: (Eq a, Num a) => a

pattern GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX = 0x8174