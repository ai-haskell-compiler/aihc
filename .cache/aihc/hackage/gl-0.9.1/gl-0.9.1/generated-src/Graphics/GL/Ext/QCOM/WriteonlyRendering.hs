-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.WriteonlyRendering (
  -- * Extension Support
    gl_QCOM_writeonly_rendering

  -- * GL_QCOM_writeonly_rendering
  , pattern GL_WRITEONLY_RENDERING_QCOM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_writeonly_rendering.txt GL_QCOM_writeonly_rendering> extension is available.

gl_QCOM_writeonly_rendering :: Bool
gl_QCOM_writeonly_rendering = member "GL_QCOM_writeonly_rendering" extensions
{-# NOINLINE gl_QCOM_writeonly_rendering #-}

pattern GL_WRITEONLY_RENDERING_QCOM  :: (Eq a, Num a) => a

pattern GL_WRITEONLY_RENDERING_QCOM = 0x8823