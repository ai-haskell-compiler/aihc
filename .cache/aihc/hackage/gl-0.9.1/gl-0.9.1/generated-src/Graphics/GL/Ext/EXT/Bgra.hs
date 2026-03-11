-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Bgra (
  -- * Extension Support
    gl_EXT_bgra

  -- * GL_EXT_bgra
  , pattern GL_BGRA_EXT
  , pattern GL_BGR_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/bgra.txt GL_EXT_bgra> extension is available.

gl_EXT_bgra :: Bool
gl_EXT_bgra = member "GL_EXT_bgra" extensions
{-# NOINLINE gl_EXT_bgra #-}

pattern GL_BGR_EXT  :: (Eq a, Num a) => a

pattern GL_BGR_EXT = 0x80E0