-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.DepthPassInstrument (
  -- * Extension Support
    gl_SGIX_depth_pass_instrument
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_depth_pass_instrument extension is available.

gl_SGIX_depth_pass_instrument :: Bool
gl_SGIX_depth_pass_instrument = member "GL_SGIX_depth_pass_instrument" extensions
{-# NOINLINE gl_SGIX_depth_pass_instrument #-}