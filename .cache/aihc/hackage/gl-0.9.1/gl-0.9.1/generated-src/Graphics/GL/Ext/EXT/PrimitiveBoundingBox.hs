-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.PrimitiveBoundingBox (
  -- * Extension Support
    gl_EXT_primitive_bounding_box

  -- * GL_EXT_primitive_bounding_box
  , glPrimitiveBoundingBoxEXT
  , pattern GL_PRIMITIVE_BOUNDING_BOX_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/EXT/EXT_primitive_bounding_box.txt GL_EXT_primitive_bounding_box> extension is available.

gl_EXT_primitive_bounding_box :: Bool
gl_EXT_primitive_bounding_box = member "GL_EXT_primitive_bounding_box" extensions
{-# NOINLINE gl_EXT_primitive_bounding_box #-}

-- | Usage: @'glPrimitiveBoundingBoxEXT' minX minY minZ minW maxX maxY maxZ maxW@
--
-- This command is an alias for 'Graphics.GL.Embedded32.glPrimitiveBoundingBox'.


glPrimitiveBoundingBoxEXT :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glPrimitiveBoundingBoxEXT = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glPrimitiveBoundingBoxEXTFunPtr

glPrimitiveBoundingBoxEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glPrimitiveBoundingBoxEXTFunPtr = unsafePerformIO (getProcAddress "glPrimitiveBoundingBoxEXT")

{-# NOINLINE glPrimitiveBoundingBoxEXTFunPtr #-}

pattern GL_PRIMITIVE_BOUNDING_BOX_EXT  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_BOUNDING_BOX_EXT = 0x92BE