-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureShader3 (
  -- * Extension Support
    gl_NV_texture_shader3

  -- * GL_NV_texture_shader3
  , pattern GL_DEPENDENT_HILO_TEXTURE_2D_NV
  , pattern GL_DEPENDENT_RGB_TEXTURE_3D_NV
  , pattern GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV
  , pattern GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV
  , pattern GL_DOT_PRODUCT_PASS_THROUGH_NV
  , pattern GL_DOT_PRODUCT_TEXTURE_1D_NV
  , pattern GL_FORCE_BLUE_TO_ONE_NV
  , pattern GL_HILO8_NV
  , pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV
  , pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV
  , pattern GL_OFFSET_HILO_TEXTURE_2D_NV
  , pattern GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV
  , pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV
  , pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV
  , pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV
  , pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV
  , pattern GL_SIGNED_HILO8_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_shader3.txt GL_NV_texture_shader3> extension is available.

gl_NV_texture_shader3 :: Bool
gl_NV_texture_shader3 = member "GL_NV_texture_shader3" extensions
{-# NOINLINE gl_NV_texture_shader3 #-}

pattern GL_DEPENDENT_HILO_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_DEPENDENT_HILO_TEXTURE_2D_NV = 0x8858

pattern GL_DEPENDENT_RGB_TEXTURE_3D_NV  :: (Eq a, Num a) => a

pattern GL_DEPENDENT_RGB_TEXTURE_3D_NV = 0x8859

pattern GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV  :: (Eq a, Num a) => a

pattern GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV = 0x885A

pattern GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV = 0x885D

pattern GL_DOT_PRODUCT_PASS_THROUGH_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_PASS_THROUGH_NV = 0x885B

pattern GL_DOT_PRODUCT_TEXTURE_1D_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_TEXTURE_1D_NV = 0x885C

pattern GL_FORCE_BLUE_TO_ONE_NV  :: (Eq a, Num a) => a

pattern GL_FORCE_BLUE_TO_ONE_NV = 0x8860

pattern GL_HILO8_NV  :: (Eq a, Num a) => a

pattern GL_HILO8_NV = 0x885E

pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV = 0x8856

pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV = 0x8857

pattern GL_OFFSET_HILO_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_HILO_TEXTURE_2D_NV = 0x8854

pattern GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV = 0x8855

pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV = 0x8850

pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV = 0x8851

pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV = 0x8852

pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV = 0x8853

pattern GL_SIGNED_HILO8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_HILO8_NV = 0x885F