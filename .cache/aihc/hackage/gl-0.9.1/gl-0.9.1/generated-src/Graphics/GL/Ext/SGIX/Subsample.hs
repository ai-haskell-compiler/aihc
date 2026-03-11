-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Subsample (
  -- * Extension Support
    gl_SGIX_subsample

  -- * GL_SGIX_subsample
  , pattern GL_PACK_SUBSAMPLE_RATE_SGIX
  , pattern GL_PIXEL_SUBSAMPLE_2424_SGIX
  , pattern GL_PIXEL_SUBSAMPLE_4242_SGIX
  , pattern GL_PIXEL_SUBSAMPLE_4444_SGIX
  , pattern GL_UNPACK_SUBSAMPLE_RATE_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_subsample extension is available.

gl_SGIX_subsample :: Bool
gl_SGIX_subsample = member "GL_SGIX_subsample" extensions
{-# NOINLINE gl_SGIX_subsample #-}

pattern GL_PACK_SUBSAMPLE_RATE_SGIX  :: (Eq a, Num a) => a

pattern GL_PACK_SUBSAMPLE_RATE_SGIX = 0x85A0

pattern GL_PIXEL_SUBSAMPLE_2424_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_SUBSAMPLE_2424_SGIX = 0x85A3

pattern GL_PIXEL_SUBSAMPLE_4242_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_SUBSAMPLE_4242_SGIX = 0x85A4

pattern GL_PIXEL_SUBSAMPLE_4444_SGIX  :: (Eq a, Num a) => a

pattern GL_PIXEL_SUBSAMPLE_4444_SGIX = 0x85A2

pattern GL_UNPACK_SUBSAMPLE_RATE_SGIX  :: (Eq a, Num a) => a

pattern GL_UNPACK_SUBSAMPLE_RATE_SGIX = 0x85A1