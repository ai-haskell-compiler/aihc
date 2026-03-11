-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.ColorSubtable (
  -- * Extension Support
    gl_EXT_color_subtable

  -- * GL_EXT_color_subtable
  , glColorSubTableEXT
  , glCopyColorSubTableEXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/color_subtable.txt GL_EXT_color_subtable> extension is available.

gl_EXT_color_subtable :: Bool
gl_EXT_color_subtable = member "GL_EXT_color_subtable" extensions
{-# NOINLINE gl_EXT_color_subtable #-}

-- | Usage: @'glColorSubTableEXT' target start count format type data@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @format@ is a @PixelFormat@, one of: 'Graphics.GL.Ext.EXT.Abgr.GL_ABGR_EXT', 'Graphics.GL.Internal.Shared.GL_ALPHA', 'Graphics.GL.Internal.Shared.GL_BLUE', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYKA_EXT', 'Graphics.GL.Ext.EXT.Cmyka.GL_CMYK_EXT', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEX', 'Graphics.GL.Internal.Shared.GL_DEPTH_COMPONENT', 'Graphics.GL.Internal.Shared.GL_GREEN', 'Graphics.GL.Internal.Shared.GL_LUMINANCE', 'Graphics.GL.Internal.Shared.GL_LUMINANCE_ALPHA', 'Graphics.GL.Internal.Shared.GL_RED', 'Graphics.GL.Ext.EXT.TextureRg.GL_RED_EXT', 'Graphics.GL.Internal.Shared.GL_RGB', 'Graphics.GL.Internal.Shared.GL_RGBA', 'Graphics.GL.Internal.Shared.GL_STENCIL_INDEX', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_422_SGIX', 'Graphics.GL.Ext.SGIX.Ycrcb.GL_YCRCB_444_SGIX'.
--
-- The parameter @type@ is a @PixelType@, one of: 'Graphics.GL.Internal.Shared.GL_BITMAP', 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE_3_3_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_BYTE_3_3_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_10_10_10_2', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_10_10_10_2_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT_8_8_8_8', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_INT_8_8_8_8_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_4_4_4_4', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_4_4_4_4_EXT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT_5_5_5_1', 'Graphics.GL.Ext.EXT.PackedPixels.GL_UNSIGNED_SHORT_5_5_5_1_EXT'.
--
-- The length of @data@ should be @COMPSIZE(format,type,count)@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glColorSubTable'.


glColorSubTableEXT :: MonadIO m => GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> m ()
glColorSubTableEXT = ffienumsizeisizeienumenumPtrVIOV glColorSubTableEXTFunPtr

glColorSubTableEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
glColorSubTableEXTFunPtr = unsafePerformIO (getProcAddress "glColorSubTableEXT")

{-# NOINLINE glColorSubTableEXTFunPtr #-}

-- | Usage: @'glCopyColorSubTableEXT' target start x y width@
--
-- The parameter @target@ is a @ColorTableTarget@.
--
-- The parameter @x@ is a @WinCoord@.
--
-- The parameter @y@ is a @WinCoord@.
--
-- This command is an alias for 'Graphics.GL.Ext.ARB.Imaging.glCopyColorSubTable'.


glCopyColorSubTableEXT :: MonadIO m => GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> m ()
glCopyColorSubTableEXT = ffienumsizeiintintsizeiIOV glCopyColorSubTableEXTFunPtr

glCopyColorSubTableEXTFunPtr :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())
glCopyColorSubTableEXTFunPtr = unsafePerformIO (getProcAddress "glCopyColorSubTableEXT")

{-# NOINLINE glCopyColorSubTableEXTFunPtr #-}