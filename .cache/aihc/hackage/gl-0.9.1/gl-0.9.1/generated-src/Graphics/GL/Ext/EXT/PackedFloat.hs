-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PackedFloat (
  -- * Extension Support
    gl_EXT_packed_float

  -- * GL_EXT_packed_float
  , pattern GL_R11F_G11F_B10F_EXT
  , pattern GL_RGBA_SIGNED_COMPONENTS_EXT
  , pattern GL_UNSIGNED_INT_10F_11F_11F_REV_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/packed_float.txt GL_EXT_packed_float> extension is available.

gl_EXT_packed_float :: Bool
gl_EXT_packed_float = member "GL_EXT_packed_float" extensions
{-# NOINLINE gl_EXT_packed_float #-}

pattern GL_R11F_G11F_B10F_EXT  :: (Eq a, Num a) => a

pattern GL_R11F_G11F_B10F_EXT = 0x8C3A

pattern GL_RGBA_SIGNED_COMPONENTS_EXT  :: (Eq a, Num a) => a

pattern GL_RGBA_SIGNED_COMPONENTS_EXT = 0x8C3C

pattern GL_UNSIGNED_INT_10F_11F_11F_REV_EXT  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_10F_11F_11F_REV_EXT = 0x8C3B