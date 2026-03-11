-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.WindowRectangles (
  -- * Extension Support
    gl_EXT_window_rectangles

  -- * GL_EXT_window_rectangles
  , glWindowRectanglesEXT
  , pattern GL_EXCLUSIVE_EXT
  , pattern GL_INCLUSIVE_EXT
  , pattern GL_MAX_WINDOW_RECTANGLES_EXT
  , pattern GL_NUM_WINDOW_RECTANGLES_EXT
  , pattern GL_WINDOW_RECTANGLE_EXT
  , pattern GL_WINDOW_RECTANGLE_MODE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/window_rectangles.txt GL_EXT_window_rectangles> extension is available.

gl_EXT_window_rectangles :: Bool
gl_EXT_window_rectangles = member "GL_EXT_window_rectangles" extensions
{-# NOINLINE gl_EXT_window_rectangles #-}

-- | Usage: @'glWindowRectanglesEXT' mode count box@
--
-- The length of @box@ should be @COMPSIZE(count)@.


glWindowRectanglesEXT :: MonadIO m => GLenum -> GLsizei -> Ptr GLint -> m ()
glWindowRectanglesEXT = ffienumsizeiPtrintIOV glWindowRectanglesEXTFunPtr

glWindowRectanglesEXTFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr GLint -> IO ())
glWindowRectanglesEXTFunPtr = unsafePerformIO (getProcAddress "glWindowRectanglesEXT")

{-# NOINLINE glWindowRectanglesEXTFunPtr #-}

pattern GL_EXCLUSIVE_EXT  :: (Eq a, Num a) => a

pattern GL_EXCLUSIVE_EXT = 0x8F11

pattern GL_INCLUSIVE_EXT  :: (Eq a, Num a) => a

pattern GL_INCLUSIVE_EXT = 0x8F10

pattern GL_MAX_WINDOW_RECTANGLES_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_WINDOW_RECTANGLES_EXT = 0x8F14

pattern GL_NUM_WINDOW_RECTANGLES_EXT  :: (Eq a, Num a) => a

pattern GL_NUM_WINDOW_RECTANGLES_EXT = 0x8F15

pattern GL_WINDOW_RECTANGLE_EXT  :: (Eq a, Num a) => a

pattern GL_WINDOW_RECTANGLE_EXT = 0x8F12

pattern GL_WINDOW_RECTANGLE_MODE_EXT  :: (Eq a, Num a) => a

pattern GL_WINDOW_RECTANGLE_MODE_EXT = 0x8F13