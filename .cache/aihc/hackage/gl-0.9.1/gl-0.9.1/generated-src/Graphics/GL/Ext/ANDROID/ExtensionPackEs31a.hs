-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANDROID.ExtensionPackEs31a (
  -- * Extension Support
    gl_ANDROID_extension_pack_es31a

  -- * GL_ANDROID_extension_pack_es31a
  , module Graphics.GL.Ext.KHR.Debug
  , module Graphics.GL.Ext.KHR.TextureCompressionAstcLdr
  , module Graphics.GL.Ext.KHR.BlendEquationAdvanced
  , module Graphics.GL.Ext.OES.SampleShading
  , module Graphics.GL.Ext.OES.SampleVariables
  , module Graphics.GL.Ext.OES.ShaderImageAtomic
  , module Graphics.GL.Ext.OES.ShaderMultisampleInterpolation
  , module Graphics.GL.Ext.OES.TextureStencil8
  , module Graphics.GL.Ext.OES.TextureStorageMultisample2dArray
  , module Graphics.GL.Ext.EXT.CopyImage
  , module Graphics.GL.Ext.EXT.DrawBuffersIndexed
  , module Graphics.GL.Ext.EXT.GeometryShader
  , module Graphics.GL.Ext.EXT.GpuShader5
  , module Graphics.GL.Ext.EXT.PrimitiveBoundingBox
  , module Graphics.GL.Ext.EXT.ShaderIoBlocks
  , module Graphics.GL.Ext.EXT.TessellationShader
  , module Graphics.GL.Ext.EXT.TextureBorderClamp
  , module Graphics.GL.Ext.EXT.TextureBuffer
  , module Graphics.GL.Ext.EXT.TextureCubeMapArray
  , module Graphics.GL.Ext.EXT.TextureSRGBDecode
) where

import Data.Set
import Graphics.GL.Internal.Proc

import Graphics.GL.Ext.KHR.Debug
import Graphics.GL.Ext.KHR.TextureCompressionAstcLdr
import Graphics.GL.Ext.KHR.BlendEquationAdvanced
import Graphics.GL.Ext.OES.SampleShading
import Graphics.GL.Ext.OES.SampleVariables
import Graphics.GL.Ext.OES.ShaderImageAtomic
import Graphics.GL.Ext.OES.ShaderMultisampleInterpolation
import Graphics.GL.Ext.OES.TextureStencil8
import Graphics.GL.Ext.OES.TextureStorageMultisample2dArray
import Graphics.GL.Ext.EXT.CopyImage
import Graphics.GL.Ext.EXT.DrawBuffersIndexed
import Graphics.GL.Ext.EXT.GeometryShader
import Graphics.GL.Ext.EXT.GpuShader5
import Graphics.GL.Ext.EXT.PrimitiveBoundingBox
import Graphics.GL.Ext.EXT.ShaderIoBlocks
import Graphics.GL.Ext.EXT.TessellationShader
import Graphics.GL.Ext.EXT.TextureBorderClamp
import Graphics.GL.Ext.EXT.TextureBuffer
import Graphics.GL.Ext.EXT.TextureCubeMapArray
import Graphics.GL.Ext.EXT.TextureSRGBDecode

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANDROID/ANDROID_extension_pack_es31a.txt GL_ANDROID_extension_pack_es31a> extension is available.

gl_ANDROID_extension_pack_es31a :: Bool
gl_ANDROID_extension_pack_es31a = member "GL_ANDROID_extension_pack_es31a" extensions
{-# NOINLINE gl_ANDROID_extension_pack_es31a #-}