-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ClipSpaceWScaling (
  -- * Extension Support
    gl_NV_clip_space_w_scaling

  -- * GL_NV_clip_space_w_scaling
  , glViewportPositionWScaleNV
  , pattern GL_VIEWPORT_POSITION_W_SCALE_NV
  , pattern GL_VIEWPORT_POSITION_W_SCALE_X_COEFF_NV
  , pattern GL_VIEWPORT_POSITION_W_SCALE_Y_COEFF_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/clip_space_w_scaling.txt GL_NV_clip_space_w_scaling> extension is available.

gl_NV_clip_space_w_scaling :: Bool
gl_NV_clip_space_w_scaling = member "GL_NV_clip_space_w_scaling" extensions
{-# NOINLINE gl_NV_clip_space_w_scaling #-}

-- | Usage: @'glViewportPositionWScaleNV' index xcoeff ycoeff@


glViewportPositionWScaleNV :: MonadIO m => GLuint -> GLfloat -> GLfloat -> m ()
glViewportPositionWScaleNV = ffiuintfloatfloatIOV glViewportPositionWScaleNVFunPtr

glViewportPositionWScaleNVFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
glViewportPositionWScaleNVFunPtr = unsafePerformIO (getProcAddress "glViewportPositionWScaleNV")

{-# NOINLINE glViewportPositionWScaleNVFunPtr #-}

pattern GL_VIEWPORT_POSITION_W_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_POSITION_W_SCALE_NV = 0x937C

pattern GL_VIEWPORT_POSITION_W_SCALE_X_COEFF_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_POSITION_W_SCALE_X_COEFF_NV = 0x937D

pattern GL_VIEWPORT_POSITION_W_SCALE_Y_COEFF_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_POSITION_W_SCALE_Y_COEFF_NV = 0x937E