-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.LightMaxExponent (
  -- * Extension Support
    gl_NV_light_max_exponent

  -- * GL_NV_light_max_exponent
  , pattern GL_MAX_SHININESS_NV
  , pattern GL_MAX_SPOT_EXPONENT_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/light_max_exponent.txt GL_NV_light_max_exponent> extension is available.

gl_NV_light_max_exponent :: Bool
gl_NV_light_max_exponent = member "GL_NV_light_max_exponent" extensions
{-# NOINLINE gl_NV_light_max_exponent #-}

pattern GL_MAX_SHININESS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_SHININESS_NV = 0x8504

pattern GL_MAX_SPOT_EXPONENT_NV  :: (Eq a, Num a) => a

pattern GL_MAX_SPOT_EXPONENT_NV = 0x8505