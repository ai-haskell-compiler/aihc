-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PolygonOffset (
  -- * Extension Support
    gl_EXT_polygon_offset

  -- * GL_EXT_polygon_offset
  , glPolygonOffsetEXT
  , pattern GL_POLYGON_OFFSET_BIAS_EXT
  , pattern GL_POLYGON_OFFSET_EXT
  , pattern GL_POLYGON_OFFSET_FACTOR_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/polygon_offset.txt GL_EXT_polygon_offset> extension is available.

gl_EXT_polygon_offset :: Bool
gl_EXT_polygon_offset = member "GL_EXT_polygon_offset" extensions
{-# NOINLINE gl_EXT_polygon_offset #-}

-- | Usage: @'glPolygonOffsetEXT' factor bias@


glPolygonOffsetEXT :: MonadIO m => GLfloat -> GLfloat -> m ()
glPolygonOffsetEXT = ffifloatfloatIOV glPolygonOffsetEXTFunPtr

glPolygonOffsetEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> IO ())
glPolygonOffsetEXTFunPtr = unsafePerformIO (getProcAddress "glPolygonOffsetEXT")

{-# NOINLINE glPolygonOffsetEXTFunPtr #-}

pattern GL_POLYGON_OFFSET_BIAS_EXT  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_BIAS_EXT = 0x8039

pattern GL_POLYGON_OFFSET_EXT  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_EXT = 0x8037

pattern GL_POLYGON_OFFSET_FACTOR_EXT  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_FACTOR_EXT = 0x8038