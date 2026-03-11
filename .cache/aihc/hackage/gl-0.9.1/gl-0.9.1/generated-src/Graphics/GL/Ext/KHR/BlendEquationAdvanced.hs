-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.KHR.BlendEquationAdvanced (
  -- * Extension Support
    gl_KHR_blend_equation_advanced

  -- * GL_KHR_blend_equation_advanced
  , glBlendBarrierKHR
  , pattern GL_COLORBURN_KHR
  , pattern GL_COLORDODGE_KHR
  , pattern GL_DARKEN_KHR
  , pattern GL_DIFFERENCE_KHR
  , pattern GL_EXCLUSION_KHR
  , pattern GL_HARDLIGHT_KHR
  , pattern GL_HSL_COLOR_KHR
  , pattern GL_HSL_HUE_KHR
  , pattern GL_HSL_LUMINOSITY_KHR
  , pattern GL_HSL_SATURATION_KHR
  , pattern GL_LIGHTEN_KHR
  , pattern GL_MULTIPLY_KHR
  , pattern GL_OVERLAY_KHR
  , pattern GL_SCREEN_KHR
  , pattern GL_SOFTLIGHT_KHR
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/KHR/blend_equation_advanced.txt GL_KHR_blend_equation_advanced> extension is available.

gl_KHR_blend_equation_advanced :: Bool
gl_KHR_blend_equation_advanced = member "GL_KHR_blend_equation_advanced" extensions
{-# NOINLINE gl_KHR_blend_equation_advanced #-}

-- | Usage: @'glBlendBarrierKHR'@
--
-- This command is an alias for 'Graphics.GL.Embedded32.glBlendBarrier'.


glBlendBarrierKHR :: MonadIO m => m ()
glBlendBarrierKHR = ffiIOV glBlendBarrierKHRFunPtr

glBlendBarrierKHRFunPtr :: FunPtr (IO ())
glBlendBarrierKHRFunPtr = unsafePerformIO (getProcAddress "glBlendBarrierKHR")

{-# NOINLINE glBlendBarrierKHRFunPtr #-}

pattern GL_COLORBURN_KHR  :: (Eq a, Num a) => a

pattern GL_COLORBURN_KHR = 0x929A

pattern GL_COLORDODGE_KHR  :: (Eq a, Num a) => a

pattern GL_COLORDODGE_KHR = 0x9299

pattern GL_DARKEN_KHR  :: (Eq a, Num a) => a

pattern GL_DARKEN_KHR = 0x9297

pattern GL_DIFFERENCE_KHR  :: (Eq a, Num a) => a

pattern GL_DIFFERENCE_KHR = 0x929E

pattern GL_EXCLUSION_KHR  :: (Eq a, Num a) => a

pattern GL_EXCLUSION_KHR = 0x92A0

pattern GL_HARDLIGHT_KHR  :: (Eq a, Num a) => a

pattern GL_HARDLIGHT_KHR = 0x929B

pattern GL_HSL_COLOR_KHR  :: (Eq a, Num a) => a

pattern GL_HSL_COLOR_KHR = 0x92AF

pattern GL_HSL_HUE_KHR  :: (Eq a, Num a) => a

pattern GL_HSL_HUE_KHR = 0x92AD

pattern GL_HSL_LUMINOSITY_KHR  :: (Eq a, Num a) => a

pattern GL_HSL_LUMINOSITY_KHR = 0x92B0

pattern GL_HSL_SATURATION_KHR  :: (Eq a, Num a) => a

pattern GL_HSL_SATURATION_KHR = 0x92AE

pattern GL_LIGHTEN_KHR  :: (Eq a, Num a) => a

pattern GL_LIGHTEN_KHR = 0x9298

pattern GL_MULTIPLY_KHR  :: (Eq a, Num a) => a

pattern GL_MULTIPLY_KHR = 0x9294

pattern GL_OVERLAY_KHR  :: (Eq a, Num a) => a

pattern GL_OVERLAY_KHR = 0x9296

pattern GL_SCREEN_KHR  :: (Eq a, Num a) => a

pattern GL_SCREEN_KHR = 0x9295

pattern GL_SOFTLIGHT_KHR  :: (Eq a, Num a) => a

pattern GL_SOFTLIGHT_KHR = 0x929C