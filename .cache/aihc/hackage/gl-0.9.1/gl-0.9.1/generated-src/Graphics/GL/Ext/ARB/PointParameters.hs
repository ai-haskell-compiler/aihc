-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.PointParameters (
  -- * Extension Support
    gl_ARB_point_parameters

  -- * GL_ARB_point_parameters
  , glPointParameterfARB
  , glPointParameterfvARB
  , pattern GL_POINT_DISTANCE_ATTENUATION_ARB
  , pattern GL_POINT_FADE_THRESHOLD_SIZE_ARB
  , pattern GL_POINT_SIZE_MAX_ARB
  , pattern GL_POINT_SIZE_MIN_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/point_parameters.txt GL_ARB_point_parameters> extension is available.

gl_ARB_point_parameters :: Bool
gl_ARB_point_parameters = member "GL_ARB_point_parameters" extensions
{-# NOINLINE gl_ARB_point_parameters #-}

-- | Usage: @'glPointParameterfARB' pname param@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameterf'.


glPointParameterfARB :: MonadIO m => GLenum -> GLfloat -> m ()
glPointParameterfARB = ffienumfloatIOV glPointParameterfARBFunPtr

glPointParameterfARBFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glPointParameterfARBFunPtr = unsafePerformIO (getProcAddress "glPointParameterfARB")

{-# NOINLINE glPointParameterfARBFunPtr #-}

-- | Usage: @'glPointParameterfvARB' pname params@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameterfv'.


glPointParameterfvARB :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glPointParameterfvARB = ffienumPtrfloatIOV glPointParameterfvARBFunPtr

glPointParameterfvARBFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glPointParameterfvARBFunPtr = unsafePerformIO (getProcAddress "glPointParameterfvARB")

{-# NOINLINE glPointParameterfvARBFunPtr #-}

pattern GL_POINT_DISTANCE_ATTENUATION_ARB  :: (Eq a, Num a) => a

pattern GL_POINT_DISTANCE_ATTENUATION_ARB = 0x8129

pattern GL_POINT_FADE_THRESHOLD_SIZE_ARB  :: (Eq a, Num a) => a

pattern GL_POINT_FADE_THRESHOLD_SIZE_ARB = 0x8128

pattern GL_POINT_SIZE_MAX_ARB  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_MAX_ARB = 0x8127

pattern GL_POINT_SIZE_MIN_ARB  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_MIN_ARB = 0x8126