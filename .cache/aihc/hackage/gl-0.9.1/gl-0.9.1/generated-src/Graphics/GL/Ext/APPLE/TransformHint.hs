-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.TransformHint (
  -- * Extension Support
    gl_APPLE_transform_hint

  -- * GL_APPLE_transform_hint
  , pattern GL_TRANSFORM_HINT_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/transform_hint.txt GL_APPLE_transform_hint> extension is available.

gl_APPLE_transform_hint :: Bool
gl_APPLE_transform_hint = member "GL_APPLE_transform_hint" extensions
{-# NOINLINE gl_APPLE_transform_hint #-}

pattern GL_TRANSFORM_HINT_APPLE  :: (Eq a, Num a) => a

pattern GL_TRANSFORM_HINT_APPLE = 0x85B1