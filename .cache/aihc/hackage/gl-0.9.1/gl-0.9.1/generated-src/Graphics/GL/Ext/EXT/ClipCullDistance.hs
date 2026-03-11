-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ClipCullDistance (
  -- * Extension Support
    gl_EXT_clip_cull_distance

  -- * GL_EXT_clip_cull_distance
  , pattern GL_CLIP_DISTANCE0_EXT
  , pattern GL_CLIP_DISTANCE1_EXT
  , pattern GL_CLIP_DISTANCE2_EXT
  , pattern GL_CLIP_DISTANCE3_EXT
  , pattern GL_CLIP_DISTANCE4_EXT
  , pattern GL_CLIP_DISTANCE5_EXT
  , pattern GL_CLIP_DISTANCE6_EXT
  , pattern GL_CLIP_DISTANCE7_EXT
  , pattern GL_MAX_CLIP_DISTANCES_EXT
  , pattern GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES_EXT
  , pattern GL_MAX_CULL_DISTANCES_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_clip_cull_distance.txt GL_EXT_clip_cull_distance> extension is available.

gl_EXT_clip_cull_distance :: Bool
gl_EXT_clip_cull_distance = member "GL_EXT_clip_cull_distance" extensions
{-# NOINLINE gl_EXT_clip_cull_distance #-}

pattern GL_CLIP_DISTANCE0_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE0_EXT = 0x3000

pattern GL_CLIP_DISTANCE1_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE1_EXT = 0x3001

pattern GL_CLIP_DISTANCE2_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE2_EXT = 0x3002

pattern GL_CLIP_DISTANCE3_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE3_EXT = 0x3003

pattern GL_CLIP_DISTANCE4_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE4_EXT = 0x3004

pattern GL_CLIP_DISTANCE5_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE5_EXT = 0x3005

pattern GL_CLIP_DISTANCE6_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE6_EXT = 0x3006

pattern GL_CLIP_DISTANCE7_EXT  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE7_EXT = 0x3007

pattern GL_MAX_CLIP_DISTANCES_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_CLIP_DISTANCES_EXT = 0x0D32

pattern GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES_EXT = 0x82FA

pattern GL_MAX_CULL_DISTANCES_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_CULL_DISTANCES_EXT = 0x82F9