-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OML.Resample (
  -- * Extension Support
    gl_OML_resample

  -- * GL_OML_resample
  , pattern GL_PACK_RESAMPLE_OML
  , pattern GL_RESAMPLE_AVERAGE_OML
  , pattern GL_RESAMPLE_DECIMATE_OML
  , pattern GL_RESAMPLE_REPLICATE_OML
  , pattern GL_RESAMPLE_ZERO_FILL_OML
  , pattern GL_UNPACK_RESAMPLE_OML
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/OML/resample.txt GL_OML_resample> extension is available.

gl_OML_resample :: Bool
gl_OML_resample = member "GL_OML_resample" extensions
{-# NOINLINE gl_OML_resample #-}

pattern GL_PACK_RESAMPLE_OML  :: (Eq a, Num a) => a

pattern GL_PACK_RESAMPLE_OML = 0x8984

pattern GL_RESAMPLE_AVERAGE_OML  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_AVERAGE_OML = 0x8988

pattern GL_RESAMPLE_DECIMATE_OML  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_DECIMATE_OML = 0x8989

pattern GL_RESAMPLE_REPLICATE_OML  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_REPLICATE_OML = 0x8986

pattern GL_RESAMPLE_ZERO_FILL_OML  :: (Eq a, Num a) => a

pattern GL_RESAMPLE_ZERO_FILL_OML = 0x8987

pattern GL_UNPACK_RESAMPLE_OML  :: (Eq a, Num a) => a

pattern GL_UNPACK_RESAMPLE_OML = 0x8985