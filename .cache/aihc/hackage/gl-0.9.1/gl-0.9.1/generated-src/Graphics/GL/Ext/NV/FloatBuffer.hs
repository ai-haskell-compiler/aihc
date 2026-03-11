-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FloatBuffer (
  -- * Extension Support
    gl_NV_float_buffer

  -- * GL_NV_float_buffer
  , pattern GL_FLOAT_CLEAR_COLOR_VALUE_NV
  , pattern GL_FLOAT_R16_NV
  , pattern GL_FLOAT_R32_NV
  , pattern GL_FLOAT_RG16_NV
  , pattern GL_FLOAT_RG32_NV
  , pattern GL_FLOAT_RGB16_NV
  , pattern GL_FLOAT_RGB32_NV
  , pattern GL_FLOAT_RGBA16_NV
  , pattern GL_FLOAT_RGBA32_NV
  , pattern GL_FLOAT_RGBA_MODE_NV
  , pattern GL_FLOAT_RGBA_NV
  , pattern GL_FLOAT_RGB_NV
  , pattern GL_FLOAT_RG_NV
  , pattern GL_FLOAT_R_NV
  , pattern GL_TEXTURE_FLOAT_COMPONENTS_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/float_buffer.txt GL_NV_float_buffer> extension is available.

gl_NV_float_buffer :: Bool
gl_NV_float_buffer = member "GL_NV_float_buffer" extensions
{-# NOINLINE gl_NV_float_buffer #-}

pattern GL_FLOAT_CLEAR_COLOR_VALUE_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_CLEAR_COLOR_VALUE_NV = 0x888D

pattern GL_FLOAT_R16_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_R16_NV = 0x8884

pattern GL_FLOAT_R32_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_R32_NV = 0x8885

pattern GL_FLOAT_RG16_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RG16_NV = 0x8886

pattern GL_FLOAT_RG32_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RG32_NV = 0x8887

pattern GL_FLOAT_RGB16_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGB16_NV = 0x8888

pattern GL_FLOAT_RGB32_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGB32_NV = 0x8889

pattern GL_FLOAT_RGBA16_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGBA16_NV = 0x888A

pattern GL_FLOAT_RGBA32_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGBA32_NV = 0x888B

pattern GL_FLOAT_RGBA_MODE_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGBA_MODE_NV = 0x888E

pattern GL_FLOAT_RGBA_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGBA_NV = 0x8883

pattern GL_FLOAT_RGB_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RGB_NV = 0x8882

pattern GL_FLOAT_RG_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_RG_NV = 0x8881

pattern GL_FLOAT_R_NV  :: (Eq a, Num a) => a

pattern GL_FLOAT_R_NV = 0x8880

pattern GL_TEXTURE_FLOAT_COMPONENTS_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_FLOAT_COMPONENTS_NV = 0x888C