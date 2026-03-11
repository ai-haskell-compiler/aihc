-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.YcrcbSubsample (
  -- * Extension Support
    gl_SGIX_ycrcb_subsample
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_ycrcb_subsample extension is available.

gl_SGIX_ycrcb_subsample :: Bool
gl_SGIX_ycrcb_subsample = member "GL_SGIX_ycrcb_subsample" extensions
{-# NOINLINE gl_SGIX_ycrcb_subsample #-}