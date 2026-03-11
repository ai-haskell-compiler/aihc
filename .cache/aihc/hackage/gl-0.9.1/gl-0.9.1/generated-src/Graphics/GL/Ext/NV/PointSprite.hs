-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PointSprite (
  -- * Extension Support
    gl_NV_point_sprite

  -- * GL_NV_point_sprite
  , glPointParameteriNV
  , glPointParameterivNV
  , pattern GL_COORD_REPLACE_NV
  , pattern GL_POINT_SPRITE_NV
  , pattern GL_POINT_SPRITE_R_MODE_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/point_sprite.txt GL_NV_point_sprite> extension is available.

gl_NV_point_sprite :: Bool
gl_NV_point_sprite = member "GL_NV_point_sprite" extensions
{-# NOINLINE gl_NV_point_sprite #-}

-- | Usage: @'glPointParameteriNV' pname param@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameteri'.


glPointParameteriNV :: MonadIO m => GLenum -> GLint -> m ()
glPointParameteriNV = ffienumintIOV glPointParameteriNVFunPtr

glPointParameteriNVFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glPointParameteriNVFunPtr = unsafePerformIO (getProcAddress "glPointParameteriNV")

{-# NOINLINE glPointParameteriNVFunPtr #-}

-- | Usage: @'glPointParameterivNV' pname params@
--
-- The parameter @pname@ is a @PointParameterNameARB@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPointParameteriv'.


glPointParameterivNV :: MonadIO m => GLenum -> Ptr GLint -> m ()
glPointParameterivNV = ffienumPtrintIOV glPointParameterivNVFunPtr

glPointParameterivNVFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glPointParameterivNVFunPtr = unsafePerformIO (getProcAddress "glPointParameterivNV")

{-# NOINLINE glPointParameterivNVFunPtr #-}

pattern GL_COORD_REPLACE_NV  :: (Eq a, Num a) => a

pattern GL_COORD_REPLACE_NV = 0x8862

pattern GL_POINT_SPRITE_NV  :: (Eq a, Num a) => a

pattern GL_POINT_SPRITE_NV = 0x8861

pattern GL_POINT_SPRITE_R_MODE_NV  :: (Eq a, Num a) => a

pattern GL_POINT_SPRITE_R_MODE_NV = 0x8863