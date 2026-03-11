-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.FragmentCoverageToColor (
  -- * Extension Support
    gl_NV_fragment_coverage_to_color

  -- * GL_NV_fragment_coverage_to_color
  , glFragmentCoverageColorNV
  , pattern GL_FRAGMENT_COVERAGE_COLOR_NV
  , pattern GL_FRAGMENT_COVERAGE_TO_COLOR_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/fragment_coverage_to_color.txt GL_NV_fragment_coverage_to_color> extension is available.

gl_NV_fragment_coverage_to_color :: Bool
gl_NV_fragment_coverage_to_color = member "GL_NV_fragment_coverage_to_color" extensions
{-# NOINLINE gl_NV_fragment_coverage_to_color #-}

-- | Usage: @'glFragmentCoverageColorNV' color@


glFragmentCoverageColorNV :: MonadIO m => GLuint -> m ()
glFragmentCoverageColorNV = ffiuintIOV glFragmentCoverageColorNVFunPtr

glFragmentCoverageColorNVFunPtr :: FunPtr (GLuint -> IO ())
glFragmentCoverageColorNVFunPtr = unsafePerformIO (getProcAddress "glFragmentCoverageColorNV")

{-# NOINLINE glFragmentCoverageColorNVFunPtr #-}

pattern GL_FRAGMENT_COVERAGE_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_COVERAGE_COLOR_NV = 0x92DE

pattern GL_FRAGMENT_COVERAGE_TO_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_COVERAGE_TO_COLOR_NV = 0x92DD