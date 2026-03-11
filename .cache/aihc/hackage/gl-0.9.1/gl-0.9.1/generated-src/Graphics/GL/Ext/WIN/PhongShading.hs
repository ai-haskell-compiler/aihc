-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.WIN.PhongShading (
  -- * Extension Support
    gl_WIN_phong_shading

  -- * GL_WIN_phong_shading
  , pattern GL_PHONG_HINT_WIN
  , pattern GL_PHONG_WIN
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/WIN/phong_shading.txt GL_WIN_phong_shading> extension is available.

gl_WIN_phong_shading :: Bool
gl_WIN_phong_shading = member "GL_WIN_phong_shading" extensions
{-# NOINLINE gl_WIN_phong_shading #-}

pattern GL_PHONG_HINT_WIN  :: (Eq a, Num a) => a

pattern GL_PHONG_HINT_WIN = 0x80EB

pattern GL_PHONG_WIN  :: (Eq a, Num a) => a

pattern GL_PHONG_WIN = 0x80EA