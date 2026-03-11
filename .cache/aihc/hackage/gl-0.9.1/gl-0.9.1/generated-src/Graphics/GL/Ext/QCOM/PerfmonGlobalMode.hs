-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.PerfmonGlobalMode (
  -- * Extension Support
    gl_QCOM_perfmon_global_mode

  -- * GL_QCOM_perfmon_global_mode
  , pattern GL_PERFMON_GLOBAL_MODE_QCOM
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_QCOM_perfmon_global_mode extension is available.

gl_QCOM_perfmon_global_mode :: Bool
gl_QCOM_perfmon_global_mode = member "GL_QCOM_perfmon_global_mode" extensions
{-# NOINLINE gl_QCOM_perfmon_global_mode #-}

pattern GL_PERFMON_GLOBAL_MODE_QCOM  :: (Eq a, Num a) => a

pattern GL_PERFMON_GLOBAL_MODE_QCOM = 0x8FA0