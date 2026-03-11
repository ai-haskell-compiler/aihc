-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.AuxDepthStencil (
  -- * Extension Support
    gl_APPLE_aux_depth_stencil

  -- * GL_APPLE_aux_depth_stencil
  , pattern GL_AUX_DEPTH_STENCIL_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/aux_depth_stencil.txt GL_APPLE_aux_depth_stencil> extension is available.

gl_APPLE_aux_depth_stencil :: Bool
gl_APPLE_aux_depth_stencil = member "GL_APPLE_aux_depth_stencil" extensions
{-# NOINLINE gl_APPLE_aux_depth_stencil #-}

pattern GL_AUX_DEPTH_STENCIL_APPLE  :: (Eq a, Num a) => a

pattern GL_AUX_DEPTH_STENCIL_APPLE = 0x8A14