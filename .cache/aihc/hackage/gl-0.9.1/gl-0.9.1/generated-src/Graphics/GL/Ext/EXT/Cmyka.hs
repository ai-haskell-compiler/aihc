-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Cmyka (
  -- * Extension Support
    gl_EXT_cmyka

  -- * GL_EXT_cmyka
  , pattern GL_CMYKA_EXT
  , pattern GL_CMYK_EXT
  , pattern GL_PACK_CMYK_HINT_EXT
  , pattern GL_UNPACK_CMYK_HINT_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/cmyka.txt GL_EXT_cmyka> extension is available.

gl_EXT_cmyka :: Bool
gl_EXT_cmyka = member "GL_EXT_cmyka" extensions
{-# NOINLINE gl_EXT_cmyka #-}

pattern GL_CMYKA_EXT  :: (Eq a, Num a) => a

pattern GL_CMYKA_EXT = 0x800D

pattern GL_CMYK_EXT  :: (Eq a, Num a) => a

pattern GL_CMYK_EXT = 0x800C

pattern GL_PACK_CMYK_HINT_EXT  :: (Eq a, Num a) => a

pattern GL_PACK_CMYK_HINT_EXT = 0x800E

pattern GL_UNPACK_CMYK_HINT_EXT  :: (Eq a, Num a) => a

pattern GL_UNPACK_CMYK_HINT_EXT = 0x800F