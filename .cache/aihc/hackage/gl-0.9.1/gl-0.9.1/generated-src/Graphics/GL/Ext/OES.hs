-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.OES (
  -- * OES Extensions
    module Graphics.GL.Ext.OES.BlendEquationSeparate
  , module Graphics.GL.Ext.OES.BlendFuncSeparate
  , module Graphics.GL.Ext.OES.BlendSubtract
  , module Graphics.GL.Ext.OES.ByteCoordinates
  , module Graphics.GL.Ext.OES.CompressedETC1RGB8SubTexture
  , module Graphics.GL.Ext.OES.CompressedETC1RGB8Texture
  , module Graphics.GL.Ext.OES.CompressedPalettedTexture
  , module Graphics.GL.Ext.OES.CopyImage
  , module Graphics.GL.Ext.OES.Depth24
  , module Graphics.GL.Ext.OES.Depth32
  , module Graphics.GL.Ext.OES.DepthTexture
  , module Graphics.GL.Ext.OES.DrawBuffersIndexed
  , module Graphics.GL.Ext.OES.DrawElementsBaseVertex
  , module Graphics.GL.Ext.OES.DrawTexture
  , module Graphics.GL.Ext.OES.EGLImage
  , module Graphics.GL.Ext.OES.EGLImageExternal
  , module Graphics.GL.Ext.OES.EGLImageExternalEssl3
  , module Graphics.GL.Ext.OES.ElementIndexUint
  , module Graphics.GL.Ext.OES.ExtendedMatrixPalette
  , module Graphics.GL.Ext.OES.FboRenderMipmap
  , module Graphics.GL.Ext.OES.FixedPoint
  , module Graphics.GL.Ext.OES.FragmentPrecisionHigh
  , module Graphics.GL.Ext.OES.FramebufferObject
  , module Graphics.GL.Ext.OES.GeometryPointSize
  , module Graphics.GL.Ext.OES.GeometryShader
  , module Graphics.GL.Ext.OES.GetProgramBinary
  , module Graphics.GL.Ext.OES.GpuShader5
  , module Graphics.GL.Ext.OES.Mapbuffer
  , module Graphics.GL.Ext.OES.MatrixGet
  , module Graphics.GL.Ext.OES.MatrixPalette
  , module Graphics.GL.Ext.OES.PackedDepthStencil
  , module Graphics.GL.Ext.OES.PointSizeArray
  , module Graphics.GL.Ext.OES.PointSprite
  , module Graphics.GL.Ext.OES.PrimitiveBoundingBox
  , module Graphics.GL.Ext.OES.QueryMatrix
  , module Graphics.GL.Ext.OES.ReadFormat
  , module Graphics.GL.Ext.OES.RequiredInternalformat
  , module Graphics.GL.Ext.OES.Rgb8Rgba8
  , module Graphics.GL.Ext.OES.SampleShading
  , module Graphics.GL.Ext.OES.SampleVariables
  , module Graphics.GL.Ext.OES.ShaderImageAtomic
  , module Graphics.GL.Ext.OES.ShaderIoBlocks
  , module Graphics.GL.Ext.OES.ShaderMultisampleInterpolation
  , module Graphics.GL.Ext.OES.SinglePrecision
  , module Graphics.GL.Ext.OES.StandardDerivatives
  , module Graphics.GL.Ext.OES.Stencil1
  , module Graphics.GL.Ext.OES.Stencil4
  , module Graphics.GL.Ext.OES.Stencil8
  , module Graphics.GL.Ext.OES.StencilWrap
  , module Graphics.GL.Ext.OES.SurfacelessContext
  , module Graphics.GL.Ext.OES.TessellationPointSize
  , module Graphics.GL.Ext.OES.TessellationShader
  , module Graphics.GL.Ext.OES.Texture3D
  , module Graphics.GL.Ext.OES.TextureBorderClamp
  , module Graphics.GL.Ext.OES.TextureBuffer
  , module Graphics.GL.Ext.OES.TextureCompressionAstc
  , module Graphics.GL.Ext.OES.TextureCubeMap
  , module Graphics.GL.Ext.OES.TextureCubeMapArray
  , module Graphics.GL.Ext.OES.TextureEnvCrossbar
  , module Graphics.GL.Ext.OES.TextureFloat
  , module Graphics.GL.Ext.OES.TextureFloatLinear
  , module Graphics.GL.Ext.OES.TextureHalfFloat
  , module Graphics.GL.Ext.OES.TextureHalfFloatLinear
  , module Graphics.GL.Ext.OES.TextureMirroredRepeat
  , module Graphics.GL.Ext.OES.TextureNpot
  , module Graphics.GL.Ext.OES.TextureStencil8
  , module Graphics.GL.Ext.OES.TextureStorageMultisample2dArray
  , module Graphics.GL.Ext.OES.TextureView
  , module Graphics.GL.Ext.OES.VertexArrayObject
  , module Graphics.GL.Ext.OES.VertexHalfFloat
  , module Graphics.GL.Ext.OES.VertexType1010102
  , module Graphics.GL.Ext.OES.ViewportArray
) where

import Graphics.GL.Ext.OES.BlendEquationSeparate
import Graphics.GL.Ext.OES.BlendFuncSeparate
import Graphics.GL.Ext.OES.BlendSubtract
import Graphics.GL.Ext.OES.ByteCoordinates
import Graphics.GL.Ext.OES.CompressedETC1RGB8SubTexture
import Graphics.GL.Ext.OES.CompressedETC1RGB8Texture
import Graphics.GL.Ext.OES.CompressedPalettedTexture
import Graphics.GL.Ext.OES.CopyImage
import Graphics.GL.Ext.OES.Depth24
import Graphics.GL.Ext.OES.Depth32
import Graphics.GL.Ext.OES.DepthTexture
import Graphics.GL.Ext.OES.DrawBuffersIndexed
import Graphics.GL.Ext.OES.DrawElementsBaseVertex
import Graphics.GL.Ext.OES.DrawTexture
import Graphics.GL.Ext.OES.EGLImage
import Graphics.GL.Ext.OES.EGLImageExternal
import Graphics.GL.Ext.OES.EGLImageExternalEssl3
import Graphics.GL.Ext.OES.ElementIndexUint
import Graphics.GL.Ext.OES.ExtendedMatrixPalette
import Graphics.GL.Ext.OES.FboRenderMipmap
import Graphics.GL.Ext.OES.FixedPoint
import Graphics.GL.Ext.OES.FragmentPrecisionHigh
import Graphics.GL.Ext.OES.FramebufferObject
import Graphics.GL.Ext.OES.GeometryPointSize
import Graphics.GL.Ext.OES.GeometryShader
import Graphics.GL.Ext.OES.GetProgramBinary
import Graphics.GL.Ext.OES.GpuShader5
import Graphics.GL.Ext.OES.Mapbuffer
import Graphics.GL.Ext.OES.MatrixGet
import Graphics.GL.Ext.OES.MatrixPalette
import Graphics.GL.Ext.OES.PackedDepthStencil
import Graphics.GL.Ext.OES.PointSizeArray
import Graphics.GL.Ext.OES.PointSprite
import Graphics.GL.Ext.OES.PrimitiveBoundingBox
import Graphics.GL.Ext.OES.QueryMatrix
import Graphics.GL.Ext.OES.ReadFormat
import Graphics.GL.Ext.OES.RequiredInternalformat
import Graphics.GL.Ext.OES.Rgb8Rgba8
import Graphics.GL.Ext.OES.SampleShading
import Graphics.GL.Ext.OES.SampleVariables
import Graphics.GL.Ext.OES.ShaderImageAtomic
import Graphics.GL.Ext.OES.ShaderIoBlocks
import Graphics.GL.Ext.OES.ShaderMultisampleInterpolation
import Graphics.GL.Ext.OES.SinglePrecision
import Graphics.GL.Ext.OES.StandardDerivatives
import Graphics.GL.Ext.OES.Stencil1
import Graphics.GL.Ext.OES.Stencil4
import Graphics.GL.Ext.OES.Stencil8
import Graphics.GL.Ext.OES.StencilWrap
import Graphics.GL.Ext.OES.SurfacelessContext
import Graphics.GL.Ext.OES.TessellationPointSize
import Graphics.GL.Ext.OES.TessellationShader
import Graphics.GL.Ext.OES.Texture3D
import Graphics.GL.Ext.OES.TextureBorderClamp
import Graphics.GL.Ext.OES.TextureBuffer
import Graphics.GL.Ext.OES.TextureCompressionAstc
import Graphics.GL.Ext.OES.TextureCubeMap
import Graphics.GL.Ext.OES.TextureCubeMapArray
import Graphics.GL.Ext.OES.TextureEnvCrossbar
import Graphics.GL.Ext.OES.TextureFloat
import Graphics.GL.Ext.OES.TextureFloatLinear
import Graphics.GL.Ext.OES.TextureHalfFloat
import Graphics.GL.Ext.OES.TextureHalfFloatLinear
import Graphics.GL.Ext.OES.TextureMirroredRepeat
import Graphics.GL.Ext.OES.TextureNpot
import Graphics.GL.Ext.OES.TextureStencil8
import Graphics.GL.Ext.OES.TextureStorageMultisample2dArray
import Graphics.GL.Ext.OES.TextureView
import Graphics.GL.Ext.OES.VertexArrayObject
import Graphics.GL.Ext.OES.VertexHalfFloat
import Graphics.GL.Ext.OES.VertexType1010102
import Graphics.GL.Ext.OES.ViewportArray