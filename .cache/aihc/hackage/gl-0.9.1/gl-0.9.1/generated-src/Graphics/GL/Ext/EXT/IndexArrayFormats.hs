-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.IndexArrayFormats (
  -- * Extension Support
    gl_EXT_index_array_formats

  -- * GL_EXT_index_array_formats
  , pattern GL_IUI_N3F_V2F_EXT
  , pattern GL_IUI_N3F_V3F_EXT
  , pattern GL_IUI_V2F_EXT
  , pattern GL_IUI_V3F_EXT
  , pattern GL_T2F_IUI_N3F_V2F_EXT
  , pattern GL_T2F_IUI_N3F_V3F_EXT
  , pattern GL_T2F_IUI_V2F_EXT
  , pattern GL_T2F_IUI_V3F_EXT
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/index_array_formats.txt GL_EXT_index_array_formats> extension is available.

gl_EXT_index_array_formats :: Bool
gl_EXT_index_array_formats = member "GL_EXT_index_array_formats" extensions
{-# NOINLINE gl_EXT_index_array_formats #-}

pattern GL_IUI_N3F_V2F_EXT  :: (Eq a, Num a) => a

pattern GL_IUI_N3F_V2F_EXT = 0x81AF

pattern GL_IUI_N3F_V3F_EXT  :: (Eq a, Num a) => a

pattern GL_IUI_N3F_V3F_EXT = 0x81B0

pattern GL_IUI_V2F_EXT  :: (Eq a, Num a) => a

pattern GL_IUI_V2F_EXT = 0x81AD

pattern GL_IUI_V3F_EXT  :: (Eq a, Num a) => a

pattern GL_IUI_V3F_EXT = 0x81AE

pattern GL_T2F_IUI_N3F_V2F_EXT  :: (Eq a, Num a) => a

pattern GL_T2F_IUI_N3F_V2F_EXT = 0x81B3

pattern GL_T2F_IUI_N3F_V3F_EXT  :: (Eq a, Num a) => a

pattern GL_T2F_IUI_N3F_V3F_EXT = 0x81B4

pattern GL_T2F_IUI_V2F_EXT  :: (Eq a, Num a) => a

pattern GL_T2F_IUI_V2F_EXT = 0x81B1

pattern GL_T2F_IUI_V3F_EXT  :: (Eq a, Num a) => a

pattern GL_T2F_IUI_V3F_EXT = 0x81B2