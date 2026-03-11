-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PointParameters (
  -- * Extension Support
    gl_EXT_point_parameters

  -- * GL_EXT_point_parameters
  , glPointParameterfEXT
  , glPointParameterfvEXT
  , pattern GL_DISTANCE_ATTENUATION_EXT
  , pattern GL_POINT_FADE_THRESHOLD_SIZE_EXT
  , pattern GL_POINT_SIZE_MAX_EXT
  , pattern GL_POINT_SIZE_MIN_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/point_parameters.txt GL_EXT_point_parameters> extension is available.

gl_EXT_point_parameters :: Bool
gl_EXT_point_parameters = member "GL_EXT_point_parameters" extensions
{-# NOINLINE gl_EXT_point_parameters #-}

-- | Usage: @'glPointParameterfEXT' pname param@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The parameter @param@ is a @CheckedFloat32@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameterf'.


glPointParameterfEXT :: MonadIO m => GLenum -> GLfloat -> m ()
glPointParameterfEXT = ffienumfloatIOV glPointParameterfEXTFunPtr

glPointParameterfEXTFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glPointParameterfEXTFunPtr = unsafePerformIO (getProcAddress "glPointParameterfEXT")

{-# NOINLINE glPointParameterfEXTFunPtr #-}

-- | Usage: @'glPointParameterfvEXT' pname params@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameterfv'.


glPointParameterfvEXT :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glPointParameterfvEXT = ffienumPtrfloatIOV glPointParameterfvEXTFunPtr

glPointParameterfvEXTFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glPointParameterfvEXTFunPtr = unsafePerformIO (getProcAddress "glPointParameterfvEXT")

{-# NOINLINE glPointParameterfvEXTFunPtr #-}

pattern GL_DISTANCE_ATTENUATION_EXT  :: (Eq a, Num a) => a

pattern GL_DISTANCE_ATTENUATION_EXT = 0x8129

pattern GL_POINT_FADE_THRESHOLD_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_POINT_FADE_THRESHOLD_SIZE_EXT = 0x8128

pattern GL_POINT_SIZE_MAX_EXT  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_MAX_EXT = 0x8127

pattern GL_POINT_SIZE_MIN_EXT  :: (Eq a, Num a) => a

pattern GL_POINT_SIZE_MIN_EXT = 0x8126