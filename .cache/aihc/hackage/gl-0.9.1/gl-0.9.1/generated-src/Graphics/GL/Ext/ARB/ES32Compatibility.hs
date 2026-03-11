-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.ES32Compatibility (
  -- * Extension Support
    gl_ARB_ES3_2_compatibility

  -- * GL_ARB_ES3_2_compatibility
  , glPrimitiveBoundingBoxARB
  , pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY_ARB
  , pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE_ARB
  , pattern GL_PRIMITIVE_BOUNDING_BOX_ARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/ES3_2_compatibility.txt GL_ARB_ES3_2_compatibility> extension is available.

gl_ARB_ES3_2_compatibility :: Bool
gl_ARB_ES3_2_compatibility = member "GL_ARB_ES3_2_compatibility" extensions
{-# NOINLINE gl_ARB_ES3_2_compatibility #-}

-- | Usage: @'glPrimitiveBoundingBoxARB' minX minY minZ minW maxX maxY maxZ maxW@
--
-- This command is an alias for 'Graphics.GL.Embedded32.glPrimitiveBoundingBox'.


glPrimitiveBoundingBoxARB :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glPrimitiveBoundingBoxARB = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glPrimitiveBoundingBoxARBFunPtr

glPrimitiveBoundingBoxARBFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glPrimitiveBoundingBoxARBFunPtr = unsafePerformIO (getProcAddress "glPrimitiveBoundingBoxARB")

{-# NOINLINE glPrimitiveBoundingBoxARBFunPtr #-}

pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY_ARB  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_LINE_WIDTH_GRANULARITY_ARB = 0x9382

pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE_ARB  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_LINE_WIDTH_RANGE_ARB = 0x9381

pattern GL_PRIMITIVE_BOUNDING_BOX_ARB  :: (Eq a, Num a) => a

pattern GL_PRIMITIVE_BOUNDING_BOX_ARB = 0x92BE