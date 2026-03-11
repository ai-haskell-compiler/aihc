-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.BlendEquationAdvanced (
  -- * Extension Support
    gl_NV_blend_equation_advanced

  -- * GL_NV_blend_equation_advanced
  , glBlendBarrierNV
  , glBlendParameteriNV
  , pattern GL_BLEND_OVERLAP_NV
  , pattern GL_BLEND_PREMULTIPLIED_SRC_NV
  , pattern GL_BLUE_NV
  , pattern GL_COLORBURN_NV
  , pattern GL_COLORDODGE_NV
  , pattern GL_CONJOINT_NV
  , pattern GL_CONTRAST_NV
  , pattern GL_DARKEN_NV
  , pattern GL_DIFFERENCE_NV
  , pattern GL_DISJOINT_NV
  , pattern GL_DST_ATOP_NV
  , pattern GL_DST_IN_NV
  , pattern GL_DST_NV
  , pattern GL_DST_OUT_NV
  , pattern GL_DST_OVER_NV
  , pattern GL_EXCLUSION_NV
  , pattern GL_GREEN_NV
  , pattern GL_HARDLIGHT_NV
  , pattern GL_HARDMIX_NV
  , pattern GL_HSL_COLOR_NV
  , pattern GL_HSL_HUE_NV
  , pattern GL_HSL_LUMINOSITY_NV
  , pattern GL_HSL_SATURATION_NV
  , pattern GL_INVERT
  , pattern GL_INVERT_OVG_NV
  , pattern GL_INVERT_RGB_NV
  , pattern GL_LIGHTEN_NV
  , pattern GL_LINEARBURN_NV
  , pattern GL_LINEARDODGE_NV
  , pattern GL_LINEARLIGHT_NV
  , pattern GL_MINUS_CLAMPED_NV
  , pattern GL_MINUS_NV
  , pattern GL_MULTIPLY_NV
  , pattern GL_OVERLAY_NV
  , pattern GL_PINLIGHT_NV
  , pattern GL_PLUS_CLAMPED_ALPHA_NV
  , pattern GL_PLUS_CLAMPED_NV
  , pattern GL_PLUS_DARKER_NV
  , pattern GL_PLUS_NV
  , pattern GL_RED_NV
  , pattern GL_SCREEN_NV
  , pattern GL_SOFTLIGHT_NV
  , pattern GL_SRC_ATOP_NV
  , pattern GL_SRC_IN_NV
  , pattern GL_SRC_NV
  , pattern GL_SRC_OUT_NV
  , pattern GL_SRC_OVER_NV
  , pattern GL_UNCORRELATED_NV
  , pattern GL_VIVIDLIGHT_NV
  , pattern GL_XOR_NV
  , pattern GL_ZERO
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/blend_equation_advanced.txt GL_NV_blend_equation_advanced> extension is available.

gl_NV_blend_equation_advanced :: Bool
gl_NV_blend_equation_advanced = member "GL_NV_blend_equation_advanced" extensions
{-# NOINLINE gl_NV_blend_equation_advanced #-}

-- | Usage: @'glBlendBarrierNV'@
--
-- This command is an alias for 'Graphics.GL.Embedded32.glBlendBarrier'.


glBlendBarrierNV :: MonadIO m => m ()
glBlendBarrierNV = ffiIOV glBlendBarrierNVFunPtr

glBlendBarrierNVFunPtr :: FunPtr (IO ())
glBlendBarrierNVFunPtr = unsafePerformIO (getProcAddress "glBlendBarrierNV")

{-# NOINLINE glBlendBarrierNVFunPtr #-}

-- | Usage: @'glBlendParameteriNV' pname value@


glBlendParameteriNV :: MonadIO m => GLenum -> GLint -> m ()
glBlendParameteriNV = ffienumintIOV glBlendParameteriNVFunPtr

glBlendParameteriNVFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glBlendParameteriNVFunPtr = unsafePerformIO (getProcAddress "glBlendParameteriNV")

{-# NOINLINE glBlendParameteriNVFunPtr #-}

pattern GL_BLEND_OVERLAP_NV  :: (Eq a, Num a) => a

pattern GL_BLEND_OVERLAP_NV = 0x9281

pattern GL_BLEND_PREMULTIPLIED_SRC_NV  :: (Eq a, Num a) => a

pattern GL_BLEND_PREMULTIPLIED_SRC_NV = 0x9280

pattern GL_BLUE_NV  :: (Eq a, Num a) => a

pattern GL_BLUE_NV = 0x1905

pattern GL_COLORBURN_NV  :: (Eq a, Num a) => a

pattern GL_COLORBURN_NV = 0x929A

pattern GL_COLORDODGE_NV  :: (Eq a, Num a) => a

pattern GL_COLORDODGE_NV = 0x9299

pattern GL_CONJOINT_NV  :: (Eq a, Num a) => a

pattern GL_CONJOINT_NV = 0x9284

pattern GL_CONTRAST_NV  :: (Eq a, Num a) => a

pattern GL_CONTRAST_NV = 0x92A1

pattern GL_DARKEN_NV  :: (Eq a, Num a) => a

pattern GL_DARKEN_NV = 0x9297

pattern GL_DIFFERENCE_NV  :: (Eq a, Num a) => a

pattern GL_DIFFERENCE_NV = 0x929E

pattern GL_DISJOINT_NV  :: (Eq a, Num a) => a

pattern GL_DISJOINT_NV = 0x9283

pattern GL_DST_ATOP_NV  :: (Eq a, Num a) => a

pattern GL_DST_ATOP_NV = 0x928F

pattern GL_DST_IN_NV  :: (Eq a, Num a) => a

pattern GL_DST_IN_NV = 0x928B

pattern GL_DST_NV  :: (Eq a, Num a) => a

pattern GL_DST_NV = 0x9287

pattern GL_DST_OUT_NV  :: (Eq a, Num a) => a

pattern GL_DST_OUT_NV = 0x928D

pattern GL_DST_OVER_NV  :: (Eq a, Num a) => a

pattern GL_DST_OVER_NV = 0x9289

pattern GL_EXCLUSION_NV  :: (Eq a, Num a) => a

pattern GL_EXCLUSION_NV = 0x92A0

pattern GL_GREEN_NV  :: (Eq a, Num a) => a

pattern GL_GREEN_NV = 0x1904

pattern GL_HARDLIGHT_NV  :: (Eq a, Num a) => a

pattern GL_HARDLIGHT_NV = 0x929B

pattern GL_HARDMIX_NV  :: (Eq a, Num a) => a

pattern GL_HARDMIX_NV = 0x92A9

pattern GL_HSL_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_HSL_COLOR_NV = 0x92AF

pattern GL_HSL_HUE_NV  :: (Eq a, Num a) => a

pattern GL_HSL_HUE_NV = 0x92AD

pattern GL_HSL_LUMINOSITY_NV  :: (Eq a, Num a) => a

pattern GL_HSL_LUMINOSITY_NV = 0x92B0

pattern GL_HSL_SATURATION_NV  :: (Eq a, Num a) => a

pattern GL_HSL_SATURATION_NV = 0x92AE

pattern GL_INVERT_OVG_NV  :: (Eq a, Num a) => a

pattern GL_INVERT_OVG_NV = 0x92B4

pattern GL_INVERT_RGB_NV  :: (Eq a, Num a) => a

pattern GL_INVERT_RGB_NV = 0x92A3

pattern GL_LIGHTEN_NV  :: (Eq a, Num a) => a

pattern GL_LIGHTEN_NV = 0x9298

pattern GL_LINEARBURN_NV  :: (Eq a, Num a) => a

pattern GL_LINEARBURN_NV = 0x92A5

pattern GL_LINEARDODGE_NV  :: (Eq a, Num a) => a

pattern GL_LINEARDODGE_NV = 0x92A4

pattern GL_LINEARLIGHT_NV  :: (Eq a, Num a) => a

pattern GL_LINEARLIGHT_NV = 0x92A7

pattern GL_MINUS_CLAMPED_NV  :: (Eq a, Num a) => a

pattern GL_MINUS_CLAMPED_NV = 0x92B3

pattern GL_MINUS_NV  :: (Eq a, Num a) => a

pattern GL_MINUS_NV = 0x929F

pattern GL_MULTIPLY_NV  :: (Eq a, Num a) => a

pattern GL_MULTIPLY_NV = 0x9294

pattern GL_OVERLAY_NV  :: (Eq a, Num a) => a

pattern GL_OVERLAY_NV = 0x9296

pattern GL_PINLIGHT_NV  :: (Eq a, Num a) => a

pattern GL_PINLIGHT_NV = 0x92A8

pattern GL_PLUS_CLAMPED_ALPHA_NV  :: (Eq a, Num a) => a

pattern GL_PLUS_CLAMPED_ALPHA_NV = 0x92B2

pattern GL_PLUS_CLAMPED_NV  :: (Eq a, Num a) => a

pattern GL_PLUS_CLAMPED_NV = 0x92B1

pattern GL_PLUS_DARKER_NV  :: (Eq a, Num a) => a

pattern GL_PLUS_DARKER_NV = 0x9292

pattern GL_PLUS_NV  :: (Eq a, Num a) => a

pattern GL_PLUS_NV = 0x9291

pattern GL_RED_NV  :: (Eq a, Num a) => a

pattern GL_RED_NV = 0x1903

pattern GL_SCREEN_NV  :: (Eq a, Num a) => a

pattern GL_SCREEN_NV = 0x9295

pattern GL_SOFTLIGHT_NV  :: (Eq a, Num a) => a

pattern GL_SOFTLIGHT_NV = 0x929C

pattern GL_SRC_ATOP_NV  :: (Eq a, Num a) => a

pattern GL_SRC_ATOP_NV = 0x928E

pattern GL_SRC_IN_NV  :: (Eq a, Num a) => a

pattern GL_SRC_IN_NV = 0x928A

pattern GL_SRC_NV  :: (Eq a, Num a) => a

pattern GL_SRC_NV = 0x9286

pattern GL_SRC_OUT_NV  :: (Eq a, Num a) => a

pattern GL_SRC_OUT_NV = 0x928C

pattern GL_SRC_OVER_NV  :: (Eq a, Num a) => a

pattern GL_SRC_OVER_NV = 0x9288

pattern GL_UNCORRELATED_NV  :: (Eq a, Num a) => a

pattern GL_UNCORRELATED_NV = 0x9282

pattern GL_VIVIDLIGHT_NV  :: (Eq a, Num a) => a

pattern GL_VIVIDLIGHT_NV = 0x92A6

pattern GL_XOR_NV  :: (Eq a, Num a) => a

pattern GL_XOR_NV = 0x1506