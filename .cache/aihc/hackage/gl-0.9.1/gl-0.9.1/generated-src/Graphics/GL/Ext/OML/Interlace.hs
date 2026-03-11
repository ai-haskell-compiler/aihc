-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OML.Interlace (
  -- * Extension Support
    gl_OML_interlace

  -- * GL_OML_interlace
  , pattern GL_INTERLACE_OML
  , pattern GL_INTERLACE_READ_OML
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/OML/interlace.txt GL_OML_interlace> extension is available.

gl_OML_interlace :: Bool
gl_OML_interlace = member "GL_OML_interlace" extensions
{-# NOINLINE gl_OML_interlace #-}

pattern GL_INTERLACE_OML  :: (Eq a, Num a) => a

pattern GL_INTERLACE_OML = 0x8980

pattern GL_INTERLACE_READ_OML  :: (Eq a, Num a) => a

pattern GL_INTERLACE_READ_OML = 0x8981