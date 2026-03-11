-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.CalligraphicFragment (
  -- * Extension Support
    gl_SGIX_calligraphic_fragment

  -- * GL_SGIX_calligraphic_fragment
  , pattern GL_CALLIGRAPHIC_FRAGMENT_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_calligraphic_fragment extension is available.

gl_SGIX_calligraphic_fragment :: Bool
gl_SGIX_calligraphic_fragment = member "GL_SGIX_calligraphic_fragment" extensions
{-# NOINLINE gl_SGIX_calligraphic_fragment #-}

pattern GL_CALLIGRAPHIC_FRAGMENT_SGIX  :: (Eq a, Num a) => a

pattern GL_CALLIGRAPHIC_FRAGMENT_SGIX = 0x8183