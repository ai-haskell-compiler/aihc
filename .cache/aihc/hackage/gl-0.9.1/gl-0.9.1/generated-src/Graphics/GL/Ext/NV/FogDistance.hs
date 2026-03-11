-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FogDistance (
  -- * Extension Support
    gl_NV_fog_distance

  -- * GL_NV_fog_distance
  , pattern GL_EYE_PLANE
  , pattern GL_EYE_PLANE_ABSOLUTE_NV
  , pattern GL_EYE_RADIAL_NV
  , pattern GL_FOG_DISTANCE_MODE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/fog_distance.txt GL_NV_fog_distance> extension is available.

gl_NV_fog_distance :: Bool
gl_NV_fog_distance = member "GL_NV_fog_distance" extensions
{-# NOINLINE gl_NV_fog_distance #-}

pattern GL_EYE_PLANE_ABSOLUTE_NV  :: (Eq a, Num a) => a

pattern GL_EYE_PLANE_ABSOLUTE_NV = 0x855C

pattern GL_EYE_RADIAL_NV  :: (Eq a, Num a) => a

pattern GL_EYE_RADIAL_NV = 0x855B

pattern GL_FOG_DISTANCE_MODE_NV  :: (Eq a, Num a) => a

pattern GL_FOG_DISTANCE_MODE_NV = 0x855A