-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.S3.S3tc (
  -- * Extension Support
    gl_S3_s3tc

  -- * GL_S3_s3tc
  , pattern GL_RGB4_S3TC
  , pattern GL_RGBA4_DXT5_S3TC
  , pattern GL_RGBA4_S3TC
  , pattern GL_RGBA_DXT5_S3TC
  , pattern GL_RGBA_S3TC
  , pattern GL_RGB_S3TC
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/S3/s3tc.txt GL_S3_s3tc> extension is available.

gl_S3_s3tc :: Bool
gl_S3_s3tc = member "GL_S3_s3tc" extensions
{-# NOINLINE gl_S3_s3tc #-}

pattern GL_RGB4_S3TC  :: (Eq a, Num a) => a

pattern GL_RGB4_S3TC = 0x83A1

pattern GL_RGBA4_DXT5_S3TC  :: (Eq a, Num a) => a

pattern GL_RGBA4_DXT5_S3TC = 0x83A5

pattern GL_RGBA4_S3TC  :: (Eq a, Num a) => a

pattern GL_RGBA4_S3TC = 0x83A3

pattern GL_RGBA_DXT5_S3TC  :: (Eq a, Num a) => a

pattern GL_RGBA_DXT5_S3TC = 0x83A4

pattern GL_RGBA_S3TC  :: (Eq a, Num a) => a

pattern GL_RGBA_S3TC = 0x83A2

pattern GL_RGB_S3TC  :: (Eq a, Num a) => a

pattern GL_RGB_S3TC = 0x83A0