-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.ClipDistance (
  -- * Extension Support
    gl_APPLE_clip_distance

  -- * GL_APPLE_clip_distance
  , pattern GL_CLIP_DISTANCE0_APPLE
  , pattern GL_CLIP_DISTANCE1_APPLE
  , pattern GL_CLIP_DISTANCE2_APPLE
  , pattern GL_CLIP_DISTANCE3_APPLE
  , pattern GL_CLIP_DISTANCE4_APPLE
  , pattern GL_CLIP_DISTANCE5_APPLE
  , pattern GL_CLIP_DISTANCE6_APPLE
  , pattern GL_CLIP_DISTANCE7_APPLE
  , pattern GL_MAX_CLIP_DISTANCES_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/APPLE/APPLE_clip_distance.txt GL_APPLE_clip_distance> extension is available.

gl_APPLE_clip_distance :: Bool
gl_APPLE_clip_distance = member "GL_APPLE_clip_distance" extensions
{-# NOINLINE gl_APPLE_clip_distance #-}

pattern GL_CLIP_DISTANCE0_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE0_APPLE = 0x3000

pattern GL_CLIP_DISTANCE1_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE1_APPLE = 0x3001

pattern GL_CLIP_DISTANCE2_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE2_APPLE = 0x3002

pattern GL_CLIP_DISTANCE3_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE3_APPLE = 0x3003

pattern GL_CLIP_DISTANCE4_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE4_APPLE = 0x3004

pattern GL_CLIP_DISTANCE5_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE5_APPLE = 0x3005

pattern GL_CLIP_DISTANCE6_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE6_APPLE = 0x3006

pattern GL_CLIP_DISTANCE7_APPLE  :: (Eq a, Num a) => a

pattern GL_CLIP_DISTANCE7_APPLE = 0x3007

pattern GL_MAX_CLIP_DISTANCES_APPLE  :: (Eq a, Num a) => a

pattern GL_MAX_CLIP_DISTANCES_APPLE = 0x0D32