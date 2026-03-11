-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IBM.RasterposClip (
  -- * Extension Support
    gl_IBM_rasterpos_clip

  -- * GL_IBM_rasterpos_clip
  , pattern GL_RASTER_POSITION_UNCLIPPED_IBM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/IBM/rasterpos_clip.txt GL_IBM_rasterpos_clip> extension is available.

gl_IBM_rasterpos_clip :: Bool
gl_IBM_rasterpos_clip = member "GL_IBM_rasterpos_clip" extensions
{-# NOINLINE gl_IBM_rasterpos_clip #-}

pattern GL_RASTER_POSITION_UNCLIPPED_IBM  :: (Eq a, Num a) => a

pattern GL_RASTER_POSITION_UNCLIPPED_IBM = 0x19262