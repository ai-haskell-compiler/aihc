-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.PolygonMode (
  -- * Extension Support
    gl_NV_polygon_mode

  -- * GL_NV_polygon_mode
  , glPolygonModeNV
  , pattern GL_FILL_NV
  , pattern GL_LINE_NV
  , pattern GL_POINT_NV
  , pattern GL_POLYGON_MODE_NV
  , pattern GL_POLYGON_OFFSET_LINE_NV
  , pattern GL_POLYGON_OFFSET_POINT_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/NV/NV_polygon_mode.txt GL_NV_polygon_mode> extension is available.

gl_NV_polygon_mode :: Bool
gl_NV_polygon_mode = member "GL_NV_polygon_mode" extensions
{-# NOINLINE gl_NV_polygon_mode #-}

-- | Usage: @'glPolygonModeNV' face mode@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @mode@ is a @PolygonMode@, one of: 'Graphics.GL.Internal.Shared.GL_FILL', 'Graphics.GL.Internal.Shared.GL_LINE', 'Graphics.GL.Internal.Shared.GL_POINT'.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glPolygonMode'.


glPolygonModeNV :: MonadIO m => GLenum -> GLenum -> m ()
glPolygonModeNV = ffienumenumIOV glPolygonModeNVFunPtr

glPolygonModeNVFunPtr :: FunPtr (GLenum -> GLenum -> IO ())
glPolygonModeNVFunPtr = unsafePerformIO (getProcAddress "glPolygonModeNV")

{-# NOINLINE glPolygonModeNVFunPtr #-}

pattern GL_FILL_NV  :: (Eq a, Num a) => a

pattern GL_FILL_NV = 0x1B02

pattern GL_LINE_NV  :: (Eq a, Num a) => a

pattern GL_LINE_NV = 0x1B01

pattern GL_POINT_NV  :: (Eq a, Num a) => a

pattern GL_POINT_NV = 0x1B00

pattern GL_POLYGON_MODE_NV  :: (Eq a, Num a) => a

pattern GL_POLYGON_MODE_NV = 0x0B40

pattern GL_POLYGON_OFFSET_LINE_NV  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_LINE_NV = 0x2A02

pattern GL_POLYGON_OFFSET_POINT_NV  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_POINT_NV = 0x2A01