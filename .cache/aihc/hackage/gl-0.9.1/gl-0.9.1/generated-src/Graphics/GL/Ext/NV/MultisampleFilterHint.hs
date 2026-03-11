-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.MultisampleFilterHint (
  -- * Extension Support
    gl_NV_multisample_filter_hint

  -- * GL_NV_multisample_filter_hint
  , pattern GL_MULTISAMPLE_FILTER_HINT_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/multisample_filter_hint.txt GL_NV_multisample_filter_hint> extension is available.

gl_NV_multisample_filter_hint :: Bool
gl_NV_multisample_filter_hint = member "GL_NV_multisample_filter_hint" extensions
{-# NOINLINE gl_NV_multisample_filter_hint #-}

pattern GL_MULTISAMPLE_FILTER_HINT_NV  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_FILTER_HINT_NV = 0x8534