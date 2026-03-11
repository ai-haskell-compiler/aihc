-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES.PrimitiveBoundingBox (
  -- * Extension Support
    gl_OES_primitive_bounding_box

  -- * GL_OES_primitive_bounding_box
  , glPrimitiveBoundingBoxOES
  , pattern GL_PRIMITIVE_BOUNDING_BOX_OES
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/OES/OES_primitive_bounding_box.txt GL_OES_primitive_bounding_box> extension is available.

gl_OES_primitive_bounding_box :: Bool
gl_OES_primitive_bounding_box = member "GL_OES_primitive_bounding_box" extensions
{-# NOINLINE gl_OES_primitive_bounding_box #-}

-- | Usage: @'glPrimitiveBoundingBoxOES' minX minY minZ minW maxX maxY maxZ maxW@
--
-- This command is an alias for 'Graphics.GL.Embedded32.glPrimitiveBoundingBox'.


glPrimitiveBoundingBoxOES :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glPrimitiveBoundingBoxOES = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glPrimitiveBoundingBoxOESFunPtr

glPrimitiveBoundingBoxOESFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glPrimitiveBoundingBoxOESFunPtr = unsafePerformIO (getProcAddress "glPrimitiveBoundingBoxOES")

{-# NOINLINE glPrimitiveBoundingBoxOESFunPtr #-}

pattern GL_PRIMITIVE_BOUNDING_BOX_OES  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_BOUNDING_BOX_OES = 0x92BE