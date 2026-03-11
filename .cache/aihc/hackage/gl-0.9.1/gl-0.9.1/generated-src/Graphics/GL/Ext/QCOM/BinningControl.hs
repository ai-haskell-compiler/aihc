-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.BinningControl (
  -- * Extension Support
    gl_QCOM_binning_control

  -- * GL_QCOM_binning_control
  , pattern GL_BINNING_CONTROL_HINT_QCOM
  , pattern GL_CPU_OPTIMIZED_QCOM
  , pattern GL_GPU_OPTIMIZED_QCOM
  , pattern GL_RENDER_DIRECT_TO_FRAMEBUFFER_QCOM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_binning_control.txt GL_QCOM_binning_control> extension is available.

gl_QCOM_binning_control :: Bool
gl_QCOM_binning_control = member "GL_QCOM_binning_control" extensions
{-# NOINLINE gl_QCOM_binning_control #-}

pattern GL_BINNING_CONTROL_HINT_QCOM  :: (Eq a, Num a) => a

pattern GL_BINNING_CONTROL_HINT_QCOM = 0x8FB0

pattern GL_CPU_OPTIMIZED_QCOM  :: (Eq a, Num a) => a

pattern GL_CPU_OPTIMIZED_QCOM = 0x8FB1

pattern GL_GPU_OPTIMIZED_QCOM  :: (Eq a, Num a) => a

pattern GL_GPU_OPTIMIZED_QCOM = 0x8FB2

pattern GL_RENDER_DIRECT_TO_FRAMEBUFFER_QCOM  :: (Eq a, Num a) => a

pattern GL_RENDER_DIRECT_TO_FRAMEBUFFER_QCOM = 0x8FB3