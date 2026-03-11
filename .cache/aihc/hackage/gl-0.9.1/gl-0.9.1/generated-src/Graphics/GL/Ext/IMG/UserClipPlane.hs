-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IMG.UserClipPlane (
  -- * Extension Support
    gl_IMG_user_clip_plane

  -- * GL_IMG_user_clip_plane
  , glClipPlanefIMG
  , glClipPlanexIMG
  , pattern GL_CLIP_PLANE0_IMG
  , pattern GL_CLIP_PLANE1_IMG
  , pattern GL_CLIP_PLANE2_IMG
  , pattern GL_CLIP_PLANE3_IMG
  , pattern GL_CLIP_PLANE4_IMG
  , pattern GL_CLIP_PLANE5_IMG
  , pattern GL_MAX_CLIP_PLANES_IMG
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/IMG/IMG_user_clip_plane.txt GL_IMG_user_clip_plane> extension is available.

gl_IMG_user_clip_plane :: Bool
gl_IMG_user_clip_plane = member "GL_IMG_user_clip_plane" extensions
{-# NOINLINE gl_IMG_user_clip_plane #-}

-- | Usage: @'glClipPlanefIMG' p eqn@
--
-- The length of @eqn@ should be @4@.


glClipPlanefIMG :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glClipPlanefIMG = ffienumPtrfloatIOV glClipPlanefIMGFunPtr

glClipPlanefIMGFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glClipPlanefIMGFunPtr = unsafePerformIO (getProcAddress "glClipPlanefIMG")

{-# NOINLINE glClipPlanefIMGFunPtr #-}

-- | Usage: @'glClipPlanexIMG' p eqn@
--
-- The length of @eqn@ should be @4@.


glClipPlanexIMG :: MonadIO m => GLenum -> Ptr GLfixed -> m ()
glClipPlanexIMG = ffienumPtrfixedIOV glClipPlanexIMGFunPtr

glClipPlanexIMGFunPtr :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
glClipPlanexIMGFunPtr = unsafePerformIO (getProcAddress "glClipPlanexIMG")

{-# NOINLINE glClipPlanexIMGFunPtr #-}

pattern GL_CLIP_PLANE0_IMG  :: (Eq a, Num a) => a

pattern GL_CLIP_PLANE0_IMG = 0x3000

pattern GL_CLIP_PLANE1_IMG  :: (Eq a, Num a) => a

pattern GL_CLIP_PLANE1_IMG = 0x3001

pattern GL_CLIP_PLANE2_IMG  :: (Eq a, Num a) => a

pattern GL_CLIP_PLANE2_IMG = 0x3002

pattern GL_CLIP_PLANE3_IMG  :: (Eq a, Num a) => a

pattern GL_CLIP_PLANE3_IMG = 0x3003

pattern GL_CLIP_PLANE4_IMG  :: (Eq a, Num a) => a

pattern GL_CLIP_PLANE4_IMG = 0x3004

pattern GL_CLIP_PLANE5_IMG  :: (Eq a, Num a) => a

pattern GL_CLIP_PLANE5_IMG = 0x3005

pattern GL_MAX_CLIP_PLANES_IMG  :: (Eq a, Num a) => a

pattern GL_MAX_CLIP_PLANES_IMG = 0x0D32