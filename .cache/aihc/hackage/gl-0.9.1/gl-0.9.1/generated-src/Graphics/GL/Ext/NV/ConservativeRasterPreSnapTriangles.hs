-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.ConservativeRasterPreSnapTriangles (
  -- * Extension Support
    gl_NV_conservative_raster_pre_snap_triangles

  -- * GL_NV_conservative_raster_pre_snap_triangles
  , glConservativeRasterParameteriNV
  , pattern GL_CONSERVATIVE_RASTER_MODE_NV
  , pattern GL_CONSERVATIVE_RASTER_MODE_POST_SNAP_NV
  , pattern GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_TRIANGLES_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/conservative_raster_pre_snap_triangles.txt GL_NV_conservative_raster_pre_snap_triangles> extension is available.

gl_NV_conservative_raster_pre_snap_triangles :: Bool
gl_NV_conservative_raster_pre_snap_triangles = member "GL_NV_conservative_raster_pre_snap_triangles" extensions
{-# NOINLINE gl_NV_conservative_raster_pre_snap_triangles #-}

-- | Usage: @'glConservativeRasterParameteriNV' pname param@


glConservativeRasterParameteriNV :: MonadIO m => GLenum -> GLint -> m ()
glConservativeRasterParameteriNV = ffienumintIOV glConservativeRasterParameteriNVFunPtr

glConservativeRasterParameteriNVFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glConservativeRasterParameteriNVFunPtr = unsafePerformIO (getProcAddress "glConservativeRasterParameteriNV")

{-# NOINLINE glConservativeRasterParameteriNVFunPtr #-}

pattern GL_CONSERVATIVE_RASTER_MODE_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTER_MODE_NV = 0x954D

pattern GL_CONSERVATIVE_RASTER_MODE_POST_SNAP_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTER_MODE_POST_SNAP_NV = 0x954E

pattern GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_TRIANGLES_NV  :: (Eq a, Num a) => a

pattern GL_CONSERVATIVE_RASTER_MODE_PRE_SNAP_TRIANGLES_NV = 0x954F