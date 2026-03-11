-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Ycrcb (
  -- * Extension Support
    gl_SGIX_ycrcb

  -- * GL_SGIX_ycrcb
  , pattern GL_YCRCB_422_SGIX
  , pattern GL_YCRCB_444_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/ycrcb.txt GL_SGIX_ycrcb> extension is available.

gl_SGIX_ycrcb :: Bool
gl_SGIX_ycrcb = member "GL_SGIX_ycrcb" extensions
{-# NOINLINE gl_SGIX_ycrcb #-}

pattern GL_YCRCB_422_SGIX  :: (Eq a, Num a) => a

pattern GL_YCRCB_422_SGIX = 0x81BB

pattern GL_YCRCB_444_SGIX  :: (Eq a, Num a) => a

pattern GL_YCRCB_444_SGIX = 0x81BC