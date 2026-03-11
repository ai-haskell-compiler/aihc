-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.PointLineTexgen (
  -- * Extension Support
    gl_SGIS_point_line_texgen

  -- * GL_SGIS_point_line_texgen
  , pattern GL_EYE_DISTANCE_TO_LINE_SGIS
  , pattern GL_EYE_DISTANCE_TO_POINT_SGIS
  , pattern GL_EYE_LINE_SGIS
  , pattern GL_EYE_POINT_SGIS
  , pattern GL_OBJECT_DISTANCE_TO_LINE_SGIS
  , pattern GL_OBJECT_DISTANCE_TO_POINT_SGIS
  , pattern GL_OBJECT_LINE_SGIS
  , pattern GL_OBJECT_POINT_SGIS
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/point_line_texgen.txt GL_SGIS_point_line_texgen> extension is available.

gl_SGIS_point_line_texgen :: Bool
gl_SGIS_point_line_texgen = member "GL_SGIS_point_line_texgen" extensions
{-# NOINLINE gl_SGIS_point_line_texgen #-}

pattern GL_EYE_DISTANCE_TO_LINE_SGIS  :: (Eq a, Num a) => a

pattern GL_EYE_DISTANCE_TO_LINE_SGIS = 0x81F2

pattern GL_EYE_DISTANCE_TO_POINT_SGIS  :: (Eq a, Num a) => a

pattern GL_EYE_DISTANCE_TO_POINT_SGIS = 0x81F0

pattern GL_EYE_LINE_SGIS  :: (Eq a, Num a) => a

pattern GL_EYE_LINE_SGIS = 0x81F6

pattern GL_EYE_POINT_SGIS  :: (Eq a, Num a) => a

pattern GL_EYE_POINT_SGIS = 0x81F4

pattern GL_OBJECT_DISTANCE_TO_LINE_SGIS  :: (Eq a, Num a) => a

pattern GL_OBJECT_DISTANCE_TO_LINE_SGIS = 0x81F3

pattern GL_OBJECT_DISTANCE_TO_POINT_SGIS  :: (Eq a, Num a) => a

pattern GL_OBJECT_DISTANCE_TO_POINT_SGIS = 0x81F1

pattern GL_OBJECT_LINE_SGIS  :: (Eq a, Num a) => a

pattern GL_OBJECT_LINE_SGIS = 0x81F7

pattern GL_OBJECT_POINT_SGIS  :: (Eq a, Num a) => a

pattern GL_OBJECT_POINT_SGIS = 0x81F5