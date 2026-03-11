-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TexgenEmboss (
  -- * Extension Support
    gl_NV_texgen_emboss

  -- * GL_NV_texgen_emboss
  , pattern GL_EMBOSS_CONSTANT_NV
  , pattern GL_EMBOSS_LIGHT_NV
  , pattern GL_EMBOSS_MAP_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texgen_emboss.txt GL_NV_texgen_emboss> extension is available.

gl_NV_texgen_emboss :: Bool
gl_NV_texgen_emboss = member "GL_NV_texgen_emboss" extensions
{-# NOINLINE gl_NV_texgen_emboss #-}

pattern GL_EMBOSS_CONSTANT_NV  :: (Eq a, Num a) => a

pattern GL_EMBOSS_CONSTANT_NV = 0x855E

pattern GL_EMBOSS_LIGHT_NV  :: (Eq a, Num a) => a

pattern GL_EMBOSS_LIGHT_NV = 0x855D

pattern GL_EMBOSS_MAP_NV  :: (Eq a, Num a) => a

pattern GL_EMBOSS_MAP_NV = 0x855F