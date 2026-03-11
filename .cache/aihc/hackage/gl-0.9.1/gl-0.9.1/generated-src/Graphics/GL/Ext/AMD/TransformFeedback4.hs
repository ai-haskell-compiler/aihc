-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.AMD.TransformFeedback4 (
  -- * Extension Support
    gl_AMD_transform_feedback4

  -- * GL_AMD_transform_feedback4
  , pattern GL_STREAM_RASTERIZATION_AMD
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/AMD/transform_feedback4.txt GL_AMD_transform_feedback4> extension is available.

gl_AMD_transform_feedback4 :: Bool
gl_AMD_transform_feedback4 = member "GL_AMD_transform_feedback4" extensions
{-# NOINLINE gl_AMD_transform_feedback4 #-}

pattern GL_STREAM_RASTERIZATION_AMD  :: (Eq a, Num a) => a

pattern GL_STREAM_RASTERIZATION_AMD = 0x91A0