-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PolygonOffsetClamp (
  -- * Extension Support
    gl_EXT_polygon_offset_clamp

  -- * GL_EXT_polygon_offset_clamp
  , glPolygonOffsetClampEXT
  , pattern GL_POLYGON_OFFSET_CLAMP_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/polygon_offset_clamp.txt GL_EXT_polygon_offset_clamp> extension is available.

gl_EXT_polygon_offset_clamp :: Bool
gl_EXT_polygon_offset_clamp = member "GL_EXT_polygon_offset_clamp" extensions
{-# NOINLINE gl_EXT_polygon_offset_clamp #-}

-- | Usage: @'glPolygonOffsetClampEXT' factor units clamp@


glPolygonOffsetClampEXT :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
glPolygonOffsetClampEXT = ffifloatfloatfloatIOV glPolygonOffsetClampEXTFunPtr

glPolygonOffsetClampEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
glPolygonOffsetClampEXTFunPtr = unsafePerformIO (getProcAddress "glPolygonOffsetClampEXT")

{-# NOINLINE glPolygonOffsetClampEXTFunPtr #-}

pattern GL_POLYGON_OFFSET_CLAMP_EXT  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_CLAMP_EXT = 0x8E1B