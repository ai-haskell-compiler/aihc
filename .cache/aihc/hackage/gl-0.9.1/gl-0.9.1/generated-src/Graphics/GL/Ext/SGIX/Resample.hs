-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Resample (
  -- * Extension Support
    gl_SGIX_resample

  -- * GL_SGIX_resample
  , pattern GL_PACK_RESAMPLE_SGIX
  , pattern GL_RESAMPLE_DECIMATE_SGIX
  , pattern GL_RESAMPLE_REPLICATE_SGIX
  , pattern GL_RESAMPLE_ZERO_FILL_SGIX
  , pattern GL_UNPACK_RESAMPLE_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/resample.txt GL_SGIX_resample> extension is available.

gl_SGIX_resample :: Bool
gl_SGIX_resample = member "GL_SGIX_resample" extensions
{-# NOINLINE gl_SGIX_resample #-}

pattern GL_PACK_RESAMPLE_SGIX  :: (Eq a, Num a) => a

pattern GL_PACK_RESAMPLE_SGIX = 0x842E

pattern GL_RESAMPLE_DECIMATE_SGIX  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_DECIMATE_SGIX = 0x8430

pattern GL_RESAMPLE_REPLICATE_SGIX  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_REPLICATE_SGIX = 0x8433

pattern GL_RESAMPLE_ZERO_FILL_SGIX  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_ZERO_FILL_SGIX = 0x8434

pattern GL_UNPACK_RESAMPLE_SGIX  :: (Eq a, Num a) => a

pattern GL_UNPACK_RESAMPLE_SGIX = 0x842F