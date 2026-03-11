-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.DepthBoundsTest (
  -- * Extension Support
    gl_EXT_depth_bounds_test

  -- * GL_EXT_depth_bounds_test
  , glDepthBoundsEXT
  , pattern GL_DEPTH_BOUNDS_EXT
  , pattern GL_DEPTH_BOUNDS_TEST_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/depth_bounds_test.txt GL_EXT_depth_bounds_test> extension is available.

gl_EXT_depth_bounds_test :: Bool
gl_EXT_depth_bounds_test = member "GL_EXT_depth_bounds_test" extensions
{-# NOINLINE gl_EXT_depth_bounds_test #-}

-- | Usage: @'glDepthBoundsEXT' zmin zmax@
--
-- The parameter @zmin@ is a @ClampedFloat64@.
--
-- The parameter @zmax@ is a @ClampedFloat64@.


glDepthBoundsEXT :: MonadIO m => GLclampd -> GLclampd -> m ()
glDepthBoundsEXT = fficlampdclampdIOV glDepthBoundsEXTFunPtr

glDepthBoundsEXTFunPtr :: FunPtr (GLclampd -> GLclampd -> IO ())
glDepthBoundsEXTFunPtr = unsafePerformIO (getProcAddress "glDepthBoundsEXT")

{-# NOINLINE glDepthBoundsEXTFunPtr #-}

pattern GL_DEPTH_BOUNDS_EXT  :: (Eq a, Num a) => a

pattern GL_DEPTH_BOUNDS_EXT = 0x8891

pattern GL_DEPTH_BOUNDS_TEST_EXT  :: (Eq a, Num a) => a

pattern GL_DEPTH_BOUNDS_TEST_EXT = 0x8890