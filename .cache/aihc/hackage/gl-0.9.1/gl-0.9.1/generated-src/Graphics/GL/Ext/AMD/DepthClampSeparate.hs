-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.DepthClampSeparate (
  -- * Extension Support
    gl_AMD_depth_clamp_separate

  -- * GL_AMD_depth_clamp_separate
  , pattern GL_DEPTH_CLAMP_FAR_AMD
  , pattern GL_DEPTH_CLAMP_NEAR_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/depth_clamp_separate.txt GL_AMD_depth_clamp_separate> extension is available.

gl_AMD_depth_clamp_separate :: Bool
gl_AMD_depth_clamp_separate = member "GL_AMD_depth_clamp_separate" extensions
{-# NOINLINE gl_AMD_depth_clamp_separate #-}

pattern GL_DEPTH_CLAMP_FAR_AMD  :: (Eq a, Num a) => a

pattern GL_DEPTH_CLAMP_FAR_AMD = 0x901F

pattern GL_DEPTH_CLAMP_NEAR_AMD  :: (Eq a, Num a) => a

pattern GL_DEPTH_CLAMP_NEAR_AMD = 0x901E