-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.TextureShader (
  -- * Extension Support
    gl_NV_texture_shader

  -- * GL_NV_texture_shader
  , pattern GL_CONST_EYE_NV
  , pattern GL_CULL_FRAGMENT_NV
  , pattern GL_CULL_MODES_NV
  , pattern GL_DEPENDENT_AR_TEXTURE_2D_NV
  , pattern GL_DEPENDENT_GB_TEXTURE_2D_NV
  , pattern GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV
  , pattern GL_DOT_PRODUCT_DEPTH_REPLACE_NV
  , pattern GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV
  , pattern GL_DOT_PRODUCT_NV
  , pattern GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV
  , pattern GL_DOT_PRODUCT_TEXTURE_2D_NV
  , pattern GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV
  , pattern GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV
  , pattern GL_DSDT8_MAG8_INTENSITY8_NV
  , pattern GL_DSDT8_MAG8_NV
  , pattern GL_DSDT8_NV
  , pattern GL_DSDT_MAG_INTENSITY_NV
  , pattern GL_DSDT_MAG_NV
  , pattern GL_DSDT_MAG_VIB_NV
  , pattern GL_DSDT_NV
  , pattern GL_DS_BIAS_NV
  , pattern GL_DS_SCALE_NV
  , pattern GL_DT_BIAS_NV
  , pattern GL_DT_SCALE_NV
  , pattern GL_HILO16_NV
  , pattern GL_HILO_NV
  , pattern GL_HI_BIAS_NV
  , pattern GL_HI_SCALE_NV
  , pattern GL_LO_BIAS_NV
  , pattern GL_LO_SCALE_NV
  , pattern GL_MAGNITUDE_BIAS_NV
  , pattern GL_MAGNITUDE_SCALE_NV
  , pattern GL_OFFSET_TEXTURE_2D_BIAS_NV
  , pattern GL_OFFSET_TEXTURE_2D_MATRIX_NV
  , pattern GL_OFFSET_TEXTURE_2D_NV
  , pattern GL_OFFSET_TEXTURE_2D_SCALE_NV
  , pattern GL_OFFSET_TEXTURE_BIAS_NV
  , pattern GL_OFFSET_TEXTURE_MATRIX_NV
  , pattern GL_OFFSET_TEXTURE_RECTANGLE_NV
  , pattern GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV
  , pattern GL_OFFSET_TEXTURE_SCALE_NV
  , pattern GL_PASS_THROUGH_NV
  , pattern GL_PREVIOUS_TEXTURE_INPUT_NV
  , pattern GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV
  , pattern GL_SHADER_CONSISTENT_NV
  , pattern GL_SHADER_OPERATION_NV
  , pattern GL_SIGNED_ALPHA8_NV
  , pattern GL_SIGNED_ALPHA_NV
  , pattern GL_SIGNED_HILO16_NV
  , pattern GL_SIGNED_HILO_NV
  , pattern GL_SIGNED_INTENSITY8_NV
  , pattern GL_SIGNED_INTENSITY_NV
  , pattern GL_SIGNED_LUMINANCE8_ALPHA8_NV
  , pattern GL_SIGNED_LUMINANCE8_NV
  , pattern GL_SIGNED_LUMINANCE_ALPHA_NV
  , pattern GL_SIGNED_LUMINANCE_NV
  , pattern GL_SIGNED_RGB8_NV
  , pattern GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV
  , pattern GL_SIGNED_RGBA8_NV
  , pattern GL_SIGNED_RGBA_NV
  , pattern GL_SIGNED_RGB_NV
  , pattern GL_SIGNED_RGB_UNSIGNED_ALPHA_NV
  , pattern GL_TEXTURE_BORDER_VALUES_NV
  , pattern GL_TEXTURE_DS_SIZE_NV
  , pattern GL_TEXTURE_DT_SIZE_NV
  , pattern GL_TEXTURE_HI_SIZE_NV
  , pattern GL_TEXTURE_LO_SIZE_NV
  , pattern GL_TEXTURE_MAG_SIZE_NV
  , pattern GL_TEXTURE_SHADER_NV
  , pattern GL_UNSIGNED_INT_8_8_S8_S8_REV_NV
  , pattern GL_UNSIGNED_INT_S8_S8_8_8_NV
  , pattern GL_VIBRANCE_BIAS_NV
  , pattern GL_VIBRANCE_SCALE_NV
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/texture_shader.txt GL_NV_texture_shader> extension is available.

gl_NV_texture_shader :: Bool
gl_NV_texture_shader = member "GL_NV_texture_shader" extensions
{-# NOINLINE gl_NV_texture_shader #-}

pattern GL_CONST_EYE_NV  :: (Eq a, Num a) => a

pattern GL_CONST_EYE_NV = 0x86E5

pattern GL_CULL_FRAGMENT_NV  :: (Eq a, Num a) => a

pattern GL_CULL_FRAGMENT_NV = 0x86E7

pattern GL_CULL_MODES_NV  :: (Eq a, Num a) => a

pattern GL_CULL_MODES_NV = 0x86E0

pattern GL_DEPENDENT_AR_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_DEPENDENT_AR_TEXTURE_2D_NV = 0x86E9

pattern GL_DEPENDENT_GB_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_DEPENDENT_GB_TEXTURE_2D_NV = 0x86EA

pattern GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV = 0x86F3

pattern GL_DOT_PRODUCT_DEPTH_REPLACE_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_DEPTH_REPLACE_NV = 0x86ED

pattern GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV = 0x86F1

pattern GL_DOT_PRODUCT_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_NV = 0x86EC

pattern GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV = 0x86F2

pattern GL_DOT_PRODUCT_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_TEXTURE_2D_NV = 0x86EE

pattern GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV = 0x86F0

pattern GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV = 0x864E

pattern GL_DSDT8_MAG8_INTENSITY8_NV  :: (Eq a, Num a) => a

pattern GL_DSDT8_MAG8_INTENSITY8_NV = 0x870B

pattern GL_DSDT8_MAG8_NV  :: (Eq a, Num a) => a

pattern GL_DSDT8_MAG8_NV = 0x870A

pattern GL_DSDT8_NV  :: (Eq a, Num a) => a

pattern GL_DSDT8_NV = 0x8709

pattern GL_DSDT_MAG_INTENSITY_NV  :: (Eq a, Num a) => a

pattern GL_DSDT_MAG_INTENSITY_NV = 0x86DC

pattern GL_DSDT_MAG_NV  :: (Eq a, Num a) => a

pattern GL_DSDT_MAG_NV = 0x86F6

pattern GL_DSDT_MAG_VIB_NV  :: (Eq a, Num a) => a

pattern GL_DSDT_MAG_VIB_NV = 0x86F7

pattern GL_DSDT_NV  :: (Eq a, Num a) => a

pattern GL_DSDT_NV = 0x86F5

pattern GL_DS_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_DS_BIAS_NV = 0x8716

pattern GL_DS_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_DS_SCALE_NV = 0x8710

pattern GL_DT_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_DT_BIAS_NV = 0x8717

pattern GL_DT_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_DT_SCALE_NV = 0x8711

pattern GL_HILO16_NV  :: (Eq a, Num a) => a

pattern GL_HILO16_NV = 0x86F8

pattern GL_HILO_NV  :: (Eq a, Num a) => a

pattern GL_HILO_NV = 0x86F4

pattern GL_HI_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_HI_BIAS_NV = 0x8714

pattern GL_HI_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_HI_SCALE_NV = 0x870E

pattern GL_LO_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_LO_BIAS_NV = 0x8715

pattern GL_LO_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_LO_SCALE_NV = 0x870F

pattern GL_MAGNITUDE_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_MAGNITUDE_BIAS_NV = 0x8718

pattern GL_MAGNITUDE_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_MAGNITUDE_SCALE_NV = 0x8712

pattern GL_OFFSET_TEXTURE_2D_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_2D_BIAS_NV = 0x86E3

pattern GL_OFFSET_TEXTURE_2D_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_2D_MATRIX_NV = 0x86E1

pattern GL_OFFSET_TEXTURE_2D_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_2D_NV = 0x86E8

pattern GL_OFFSET_TEXTURE_2D_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_2D_SCALE_NV = 0x86E2

pattern GL_OFFSET_TEXTURE_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_BIAS_NV = 0x86E3

pattern GL_OFFSET_TEXTURE_MATRIX_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_MATRIX_NV = 0x86E1

pattern GL_OFFSET_TEXTURE_RECTANGLE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_RECTANGLE_NV = 0x864C

pattern GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV = 0x864D

pattern GL_OFFSET_TEXTURE_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_OFFSET_TEXTURE_SCALE_NV = 0x86E2

pattern GL_PASS_THROUGH_NV  :: (Eq a, Num a) => a

pattern GL_PASS_THROUGH_NV = 0x86E6

pattern GL_PREVIOUS_TEXTURE_INPUT_NV  :: (Eq a, Num a) => a

pattern GL_PREVIOUS_TEXTURE_INPUT_NV = 0x86E4

pattern GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV  :: (Eq a, Num a) => a

pattern GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV = 0x86D9

pattern GL_SHADER_CONSISTENT_NV  :: (Eq a, Num a) => a

pattern GL_SHADER_CONSISTENT_NV = 0x86DD

pattern GL_SHADER_OPERATION_NV  :: (Eq a, Num a) => a

pattern GL_SHADER_OPERATION_NV = 0x86DF

pattern GL_SIGNED_ALPHA8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_ALPHA8_NV = 0x8706

pattern GL_SIGNED_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_ALPHA_NV = 0x8705

pattern GL_SIGNED_HILO16_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_HILO16_NV = 0x86FA

pattern GL_SIGNED_HILO_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_HILO_NV = 0x86F9

pattern GL_SIGNED_INTENSITY8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_INTENSITY8_NV = 0x8708

pattern GL_SIGNED_INTENSITY_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_INTENSITY_NV = 0x8707

pattern GL_SIGNED_LUMINANCE8_ALPHA8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_LUMINANCE8_ALPHA8_NV = 0x8704

pattern GL_SIGNED_LUMINANCE8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_LUMINANCE8_NV = 0x8702

pattern GL_SIGNED_LUMINANCE_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_LUMINANCE_ALPHA_NV = 0x8703

pattern GL_SIGNED_LUMINANCE_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_LUMINANCE_NV = 0x8701

pattern GL_SIGNED_RGB8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_RGB8_NV = 0x86FF

pattern GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV = 0x870D

pattern GL_SIGNED_RGBA8_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_RGBA8_NV = 0x86FC

pattern GL_SIGNED_RGBA_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_RGBA_NV = 0x86FB

pattern GL_SIGNED_RGB_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_RGB_NV = 0x86FE

pattern GL_SIGNED_RGB_UNSIGNED_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_RGB_UNSIGNED_ALPHA_NV = 0x870C

pattern GL_TEXTURE_BORDER_VALUES_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_BORDER_VALUES_NV = 0x871A

pattern GL_TEXTURE_DS_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DS_SIZE_NV = 0x871D

pattern GL_TEXTURE_DT_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_DT_SIZE_NV = 0x871E

pattern GL_TEXTURE_HI_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_HI_SIZE_NV = 0x871B

pattern GL_TEXTURE_LO_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_LO_SIZE_NV = 0x871C

pattern GL_TEXTURE_MAG_SIZE_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_MAG_SIZE_NV = 0x871F

pattern GL_TEXTURE_SHADER_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_SHADER_NV = 0x86DE

pattern GL_UNSIGNED_INT_8_8_S8_S8_REV_NV  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_8_8_S8_S8_REV_NV = 0x86DB

pattern GL_UNSIGNED_INT_S8_S8_8_8_NV  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INT_S8_S8_8_8_NV = 0x86DA

pattern GL_VIBRANCE_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_VIBRANCE_BIAS_NV = 0x8719

pattern GL_VIBRANCE_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_VIBRANCE_SCALE_NV = 0x8713