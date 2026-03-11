-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.MiscAttribute (
  -- * Extension Support
    gl_EXT_misc_attribute
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/misc_attribute.txt GL_EXT_misc_attribute> extension is available.

gl_EXT_misc_attribute :: Bool
gl_EXT_misc_attribute = member "GL_EXT_misc_attribute" extensions
{-# NOINLINE gl_EXT_misc_attribute #-}