-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.Ycrcba (
  -- * Extension Support
    gl_SGIX_ycrcba

  -- * GL_SGIX_ycrcba
  , pattern GL_YCRCBA_SGIX
  , pattern GL_YCRCB_SGIX
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the GL_SGIX_ycrcba extension is available.

gl_SGIX_ycrcba :: Bool
gl_SGIX_ycrcba = member "GL_SGIX_ycrcba" extensions
{-# NOINLINE gl_SGIX_ycrcba #-}

pattern GL_YCRCBA_SGIX  :: (Eq a, Num a) => a

pattern GL_YCRCBA_SGIX = 0x8319

pattern GL_YCRCB_SGIX  :: (Eq a, Num a) => a

pattern GL_YCRCB_SGIX = 0x8318