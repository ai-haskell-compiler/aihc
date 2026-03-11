-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OML.Subsample (
  -- * Extension Support
    gl_OML_subsample

  -- * GL_OML_subsample
  , pattern GL_FORMAT_SUBSAMPLE_244_244_OML
  , pattern GL_FORMAT_SUBSAMPLE_24_24_OML
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/OML/subsample.txt GL_OML_subsample> extension is available.

gl_OML_subsample :: Bool
gl_OML_subsample = member "GL_OML_subsample" extensions
{-# NOINLINE gl_OML_subsample #-}

pattern GL_FORMAT_SUBSAMPLE_244_244_OML  :: (Eq a, Num a) => a

pattern GL_FORMAT_SUBSAMPLE_244_244_OML = 0x8983

pattern GL_FORMAT_SUBSAMPLE_24_24_OML  :: (Eq a, Num a) => a

pattern GL_FORMAT_SUBSAMPLE_24_24_OML = 0x8982