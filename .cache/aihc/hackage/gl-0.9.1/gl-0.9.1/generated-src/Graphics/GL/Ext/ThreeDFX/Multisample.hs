-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ThreeDFX.Multisample (
  -- * Extension Support
    gl_3DFX_multisample

  -- * GL_3DFX_multisample
  , pattern GL_MULTISAMPLE_3DFX
  , pattern GL_MULTISAMPLE_BIT_3DFX
  , pattern GL_SAMPLES_3DFX
  , pattern GL_SAMPLE_BUFFERS_3DFX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/3DFX/3dfx_multisample.txt GL_3DFX_multisample> extension is available.

gl_3DFX_multisample :: Bool
gl_3DFX_multisample = member "GL_3DFX_multisample" extensions
{-# NOINLINE gl_3DFX_multisample #-}

pattern GL_MULTISAMPLE_3DFX  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_3DFX = 0x86B2

pattern GL_MULTISAMPLE_BIT_3DFX  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BIT_3DFX = 0x20000000

pattern GL_SAMPLES_3DFX  :: (Eq a, Num a) => a

pattern GL_SAMPLES_3DFX = 0x86B4

pattern GL_SAMPLE_BUFFERS_3DFX  :: (Eq a, Num a) => a

pattern GL_SAMPLE_BUFFERS_3DFX = 0x86B3