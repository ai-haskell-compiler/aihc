-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.PointParameters (
  -- * Extension Support
    gl_SGIS_point_parameters

  -- * GL_SGIS_point_parameters
  , glPointParameterfSGIS
  , glPointParameterfvSGIS
  , pattern GL_DISTANCE_ATTENUATION_SGIS
  , pattern GL_POINT_FADE_THRESHOLD_SIZE_SGIS
  , pattern GL_POINT_SIZE_MAX_SGIS
  , pattern GL_POINT_SIZE_MIN_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_SGIS_point_parameters extension is available.

gl_SGIS_point_parameters :: Bool
gl_SGIS_point_parameters = member "GL_SGIS_point_parameters" extensions
{-# NOINLINE gl_SGIS_point_parameters #-}

-- | Usage: @'glPointParameterfSGIS' pname param@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameterf'.


glPointParameterfSGIS :: MonadIO m => GLenum -> GLfloat -> m ()
glPointParameterfSGIS = ffienumfloatIOV glPointParameterfSGISFunPtr

glPointParameterfSGISFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glPointParameterfSGISFunPtr = unsafePerformIO (getProcAddress "glPointParameterfSGIS")

{-# NOINLINE glPointParameterfSGISFunPtr #-}

-- | Usage: @'glPointParameterfvSGIS' pname params@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameterfv'.


glPointParameterfvSGIS :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glPointParameterfvSGIS = ffienumPtrfloatIOV glPointParameterfvSGISFunPtr

glPointParameterfvSGISFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glPointParameterfvSGISFunPtr = unsafePerformIO (getProcAddress "glPointParameterfvSGIS")

{-# NOINLINE glPointParameterfvSGISFunPtr #-}

pattern GL_DISTANCE_ATTENUATION_SGIS  :: (Eq a, Num a) => a

pattern GL_DISTANCE_ATTENUATION_SGIS = 0x8129

pattern GL_POINT_FADE_THRESHOLD_SIZE_SGIS  :: (Eq a, Num a) => a

pattern GL_POINT_FADE_THRESHOLD_SIZE_SGIS = 0x8128

pattern GL_POINT_SIZE_MAX_SGIS  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_MAX_SGIS = 0x8127

pattern GL_POINT_SIZE_MIN_SGIS  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_MIN_SGIS = 0x8126