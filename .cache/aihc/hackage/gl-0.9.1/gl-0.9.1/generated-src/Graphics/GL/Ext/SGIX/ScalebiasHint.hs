-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.ScalebiasHint (
  -- * Extension Support
    gl_SGIX_scalebias_hint

  -- * GL_SGIX_scalebias_hint
  , pattern GL_SCALEBIAS_HINT_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_scalebias_hint extension is available.

gl_SGIX_scalebias_hint :: Bool
gl_SGIX_scalebias_hint = member "GL_SGIX_scalebias_hint" extensions
{-# NOINLINE gl_SGIX_scalebias_hint #-}

pattern GL_SCALEBIAS_HINT_SGIX  :: (Eq a, Num a) => a

pattern GL_SCALEBIAS_HINT_SGIX = 0x8322