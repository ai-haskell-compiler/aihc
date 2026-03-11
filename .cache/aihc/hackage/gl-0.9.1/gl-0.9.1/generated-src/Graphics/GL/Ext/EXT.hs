-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT (
  -- * EXT Extensions
    module Graphics.GL.Ext.EXT.Abgr
  , module Graphics.GL.Ext.EXT.BaseInstance
  , module Graphics.GL.Ext.EXT.Bgra
  , module Graphics.GL.Ext.EXT.BindableUniform
  , module Graphics.GL.Ext.EXT.BlendColor
  , module Graphics.GL.Ext.EXT.BlendEquationSeparate
  , module Graphics.GL.Ext.EXT.BlendFuncExtended
  , module Graphics.GL.Ext.EXT.BlendFuncSeparate
  , module Graphics.GL.Ext.EXT.BlendLogicOp
  , module Graphics.GL.Ext.EXT.BlendMinmax
  , module Graphics.GL.Ext.EXT.BlendSubtract
  , module Graphics.GL.Ext.EXT.BufferStorage
  , module Graphics.GL.Ext.EXT.ClearTexture
  , module Graphics.GL.Ext.EXT.ClipCullDistance
  , module Graphics.GL.Ext.EXT.ClipVolumeHint
  , module Graphics.GL.Ext.EXT.Cmyka
  , module Graphics.GL.Ext.EXT.ColorBufferFloat
  , module Graphics.GL.Ext.EXT.ColorBufferHalfFloat
  , module Graphics.GL.Ext.EXT.ColorSubtable
  , module Graphics.GL.Ext.EXT.CompiledVertexArray
  , module Graphics.GL.Ext.EXT.ConservativeDepth
  , module Graphics.GL.Ext.EXT.Convolution
  , module Graphics.GL.Ext.EXT.CoordinateFrame
  , module Graphics.GL.Ext.EXT.CopyImage
  , module Graphics.GL.Ext.EXT.CopyTexture
  , module Graphics.GL.Ext.EXT.CullVertex
  , module Graphics.GL.Ext.EXT.DebugLabel
  , module Graphics.GL.Ext.EXT.DebugMarker
  , module Graphics.GL.Ext.EXT.DepthBoundsTest
  , module Graphics.GL.Ext.EXT.DirectStateAccess
  , module Graphics.GL.Ext.EXT.DiscardFramebuffer
  , module Graphics.GL.Ext.EXT.DisjointTimerQuery
  , module Graphics.GL.Ext.EXT.DrawBuffers
  , module Graphics.GL.Ext.EXT.DrawBuffers2
  , module Graphics.GL.Ext.EXT.DrawBuffersIndexed
  , module Graphics.GL.Ext.EXT.DrawElementsBaseVertex
  , module Graphics.GL.Ext.EXT.DrawInstanced
  , module Graphics.GL.Ext.EXT.DrawRangeElements
  , module Graphics.GL.Ext.EXT.DrawTransformFeedback
  , module Graphics.GL.Ext.EXT.FloatBlend
  , module Graphics.GL.Ext.EXT.FogCoord
  , module Graphics.GL.Ext.EXT.FourTwoTwoPixels
  , module Graphics.GL.Ext.EXT.FramebufferBlit
  , module Graphics.GL.Ext.EXT.FramebufferMultisample
  , module Graphics.GL.Ext.EXT.FramebufferMultisampleBlitScaled
  , module Graphics.GL.Ext.EXT.FramebufferObject
  , module Graphics.GL.Ext.EXT.FramebufferSRGB
  , module Graphics.GL.Ext.EXT.GeometryPointSize
  , module Graphics.GL.Ext.EXT.GeometryShader
  , module Graphics.GL.Ext.EXT.GeometryShader4
  , module Graphics.GL.Ext.EXT.GpuProgramParameters
  , module Graphics.GL.Ext.EXT.GpuShader4
  , module Graphics.GL.Ext.EXT.GpuShader5
  , module Graphics.GL.Ext.EXT.Histogram
  , module Graphics.GL.Ext.EXT.IndexArrayFormats
  , module Graphics.GL.Ext.EXT.IndexFunc
  , module Graphics.GL.Ext.EXT.IndexMaterial
  , module Graphics.GL.Ext.EXT.IndexTexture
  , module Graphics.GL.Ext.EXT.InstancedArrays
  , module Graphics.GL.Ext.EXT.LightTexture
  , module Graphics.GL.Ext.EXT.MapBufferRange
  , module Graphics.GL.Ext.EXT.MiscAttribute
  , module Graphics.GL.Ext.EXT.MultiDrawArrays
  , module Graphics.GL.Ext.EXT.MultiDrawIndirect
  , module Graphics.GL.Ext.EXT.Multisample
  , module Graphics.GL.Ext.EXT.MultisampleCompatibility
  , module Graphics.GL.Ext.EXT.MultisampledRenderToTexture
  , module Graphics.GL.Ext.EXT.MultiviewDrawBuffers
  , module Graphics.GL.Ext.EXT.OcclusionQueryBoolean
  , module Graphics.GL.Ext.EXT.PackedDepthStencil
  , module Graphics.GL.Ext.EXT.PackedFloat
  , module Graphics.GL.Ext.EXT.PackedPixels
  , module Graphics.GL.Ext.EXT.PalettedTexture
  , module Graphics.GL.Ext.EXT.PixelBufferObject
  , module Graphics.GL.Ext.EXT.PixelTransform
  , module Graphics.GL.Ext.EXT.PixelTransformColorTable
  , module Graphics.GL.Ext.EXT.PointParameters
  , module Graphics.GL.Ext.EXT.PolygonOffset
  , module Graphics.GL.Ext.EXT.PolygonOffsetClamp
  , module Graphics.GL.Ext.EXT.PostDepthCoverage
  , module Graphics.GL.Ext.EXT.PrimitiveBoundingBox
  , module Graphics.GL.Ext.EXT.ProtectedTextures
  , module Graphics.GL.Ext.EXT.ProvokingVertex
  , module Graphics.GL.Ext.EXT.PvrtcSRGB
  , module Graphics.GL.Ext.EXT.RasterMultisample
  , module Graphics.GL.Ext.EXT.ReadFormatBgra
  , module Graphics.GL.Ext.EXT.RenderSnorm
  , module Graphics.GL.Ext.EXT.RescaleNormal
  , module Graphics.GL.Ext.EXT.Robustness
  , module Graphics.GL.Ext.EXT.SRGB
  , module Graphics.GL.Ext.EXT.SRGBWriteControl
  , module Graphics.GL.Ext.EXT.SecondaryColor
  , module Graphics.GL.Ext.EXT.SeparateShaderObjects
  , module Graphics.GL.Ext.EXT.SeparateSpecularColor
  , module Graphics.GL.Ext.EXT.ShaderFramebufferFetch
  , module Graphics.GL.Ext.EXT.ShaderGroupVote
  , module Graphics.GL.Ext.EXT.ShaderImageLoadFormatted
  , module Graphics.GL.Ext.EXT.ShaderImageLoadStore
  , module Graphics.GL.Ext.EXT.ShaderImplicitConversions
  , module Graphics.GL.Ext.EXT.ShaderIntegerMix
  , module Graphics.GL.Ext.EXT.ShaderIoBlocks
  , module Graphics.GL.Ext.EXT.ShaderNonConstantGlobalInitializers
  , module Graphics.GL.Ext.EXT.ShaderPixelLocalStorage
  , module Graphics.GL.Ext.EXT.ShaderPixelLocalStorage2
  , module Graphics.GL.Ext.EXT.ShaderTextureLod
  , module Graphics.GL.Ext.EXT.ShadowFuncs
  , module Graphics.GL.Ext.EXT.ShadowSamplers
  , module Graphics.GL.Ext.EXT.SharedTexturePalette
  , module Graphics.GL.Ext.EXT.SparseTexture
  , module Graphics.GL.Ext.EXT.SparseTexture2
  , module Graphics.GL.Ext.EXT.StencilClearTag
  , module Graphics.GL.Ext.EXT.StencilTwoSide
  , module Graphics.GL.Ext.EXT.StencilWrap
  , module Graphics.GL.Ext.EXT.Subtexture
  , module Graphics.GL.Ext.EXT.TessellationPointSize
  , module Graphics.GL.Ext.EXT.TessellationShader
  , module Graphics.GL.Ext.EXT.Texture
  , module Graphics.GL.Ext.EXT.Texture3D
  , module Graphics.GL.Ext.EXT.TextureArray
  , module Graphics.GL.Ext.EXT.TextureBorderClamp
  , module Graphics.GL.Ext.EXT.TextureBuffer
  , module Graphics.GL.Ext.EXT.TextureBufferObject
  , module Graphics.GL.Ext.EXT.TextureCompressionDxt1
  , module Graphics.GL.Ext.EXT.TextureCompressionLatc
  , module Graphics.GL.Ext.EXT.TextureCompressionRgtc
  , module Graphics.GL.Ext.EXT.TextureCompressionS3tc
  , module Graphics.GL.Ext.EXT.TextureCubeMap
  , module Graphics.GL.Ext.EXT.TextureCubeMapArray
  , module Graphics.GL.Ext.EXT.TextureEnvAdd
  , module Graphics.GL.Ext.EXT.TextureEnvCombine
  , module Graphics.GL.Ext.EXT.TextureEnvDot3
  , module Graphics.GL.Ext.EXT.TextureFilterAnisotropic
  , module Graphics.GL.Ext.EXT.TextureFilterMinmax
  , module Graphics.GL.Ext.EXT.TextureFormatBGRA8888
  , module Graphics.GL.Ext.EXT.TextureInteger
  , module Graphics.GL.Ext.EXT.TextureLodBias
  , module Graphics.GL.Ext.EXT.TextureMirrorClamp
  , module Graphics.GL.Ext.EXT.TextureNorm16
  , module Graphics.GL.Ext.EXT.TextureObject
  , module Graphics.GL.Ext.EXT.TexturePerturbNormal
  , module Graphics.GL.Ext.EXT.TextureRg
  , module Graphics.GL.Ext.EXT.TextureSRGB
  , module Graphics.GL.Ext.EXT.TextureSRGBDecode
  , module Graphics.GL.Ext.EXT.TextureSRGBR8
  , module Graphics.GL.Ext.EXT.TextureSRGBRG8
  , module Graphics.GL.Ext.EXT.TextureSharedExponent
  , module Graphics.GL.Ext.EXT.TextureSnorm
  , module Graphics.GL.Ext.EXT.TextureStorage
  , module Graphics.GL.Ext.EXT.TextureSwizzle
  , module Graphics.GL.Ext.EXT.TextureType2101010REV
  , module Graphics.GL.Ext.EXT.TextureView
  , module Graphics.GL.Ext.EXT.TimerQuery
  , module Graphics.GL.Ext.EXT.TransformFeedback
  , module Graphics.GL.Ext.EXT.UnpackSubimage
  , module Graphics.GL.Ext.EXT.VertexArray
  , module Graphics.GL.Ext.EXT.VertexArrayBgra
  , module Graphics.GL.Ext.EXT.VertexAttrib64bit
  , module Graphics.GL.Ext.EXT.VertexShader
  , module Graphics.GL.Ext.EXT.VertexWeighting
  , module Graphics.GL.Ext.EXT.WindowRectangles
  , module Graphics.GL.Ext.EXT.X11SyncObject
  , module Graphics.GL.Ext.EXT.YUVTarget
) where

import Graphics.GL.Ext.EXT.Abgr
import Graphics.GL.Ext.EXT.BaseInstance
import Graphics.GL.Ext.EXT.Bgra
import Graphics.GL.Ext.EXT.BindableUniform
import Graphics.GL.Ext.EXT.BlendColor
import Graphics.GL.Ext.EXT.BlendEquationSeparate
import Graphics.GL.Ext.EXT.BlendFuncExtended
import Graphics.GL.Ext.EXT.BlendFuncSeparate
import Graphics.GL.Ext.EXT.BlendLogicOp
import Graphics.GL.Ext.EXT.BlendMinmax
import Graphics.GL.Ext.EXT.BlendSubtract
import Graphics.GL.Ext.EXT.BufferStorage
import Graphics.GL.Ext.EXT.ClearTexture
import Graphics.GL.Ext.EXT.ClipCullDistance
import Graphics.GL.Ext.EXT.ClipVolumeHint
import Graphics.GL.Ext.EXT.Cmyka
import Graphics.GL.Ext.EXT.ColorBufferFloat
import Graphics.GL.Ext.EXT.ColorBufferHalfFloat
import Graphics.GL.Ext.EXT.ColorSubtable
import Graphics.GL.Ext.EXT.CompiledVertexArray
import Graphics.GL.Ext.EXT.ConservativeDepth
import Graphics.GL.Ext.EXT.Convolution
import Graphics.GL.Ext.EXT.CoordinateFrame
import Graphics.GL.Ext.EXT.CopyImage
import Graphics.GL.Ext.EXT.CopyTexture
import Graphics.GL.Ext.EXT.CullVertex
import Graphics.GL.Ext.EXT.DebugLabel
import Graphics.GL.Ext.EXT.DebugMarker
import Graphics.GL.Ext.EXT.DepthBoundsTest
import Graphics.GL.Ext.EXT.DirectStateAccess
import Graphics.GL.Ext.EXT.DiscardFramebuffer
import Graphics.GL.Ext.EXT.DisjointTimerQuery
import Graphics.GL.Ext.EXT.DrawBuffers
import Graphics.GL.Ext.EXT.DrawBuffers2
import Graphics.GL.Ext.EXT.DrawBuffersIndexed
import Graphics.GL.Ext.EXT.DrawElementsBaseVertex
import Graphics.GL.Ext.EXT.DrawInstanced
import Graphics.GL.Ext.EXT.DrawRangeElements
import Graphics.GL.Ext.EXT.DrawTransformFeedback
import Graphics.GL.Ext.EXT.FloatBlend
import Graphics.GL.Ext.EXT.FogCoord
import Graphics.GL.Ext.EXT.FourTwoTwoPixels
import Graphics.GL.Ext.EXT.FramebufferBlit
import Graphics.GL.Ext.EXT.FramebufferMultisample
import Graphics.GL.Ext.EXT.FramebufferMultisampleBlitScaled
import Graphics.GL.Ext.EXT.FramebufferObject
import Graphics.GL.Ext.EXT.FramebufferSRGB
import Graphics.GL.Ext.EXT.GeometryPointSize
import Graphics.GL.Ext.EXT.GeometryShader
import Graphics.GL.Ext.EXT.GeometryShader4
import Graphics.GL.Ext.EXT.GpuProgramParameters
import Graphics.GL.Ext.EXT.GpuShader4
import Graphics.GL.Ext.EXT.GpuShader5
import Graphics.GL.Ext.EXT.Histogram
import Graphics.GL.Ext.EXT.IndexArrayFormats
import Graphics.GL.Ext.EXT.IndexFunc
import Graphics.GL.Ext.EXT.IndexMaterial
import Graphics.GL.Ext.EXT.IndexTexture
import Graphics.GL.Ext.EXT.InstancedArrays
import Graphics.GL.Ext.EXT.LightTexture
import Graphics.GL.Ext.EXT.MapBufferRange
import Graphics.GL.Ext.EXT.MiscAttribute
import Graphics.GL.Ext.EXT.MultiDrawArrays
import Graphics.GL.Ext.EXT.MultiDrawIndirect
import Graphics.GL.Ext.EXT.Multisample
import Graphics.GL.Ext.EXT.MultisampleCompatibility
import Graphics.GL.Ext.EXT.MultisampledRenderToTexture
import Graphics.GL.Ext.EXT.MultiviewDrawBuffers
import Graphics.GL.Ext.EXT.OcclusionQueryBoolean
import Graphics.GL.Ext.EXT.PackedDepthStencil
import Graphics.GL.Ext.EXT.PackedFloat
import Graphics.GL.Ext.EXT.PackedPixels
import Graphics.GL.Ext.EXT.PalettedTexture
import Graphics.GL.Ext.EXT.PixelBufferObject
import Graphics.GL.Ext.EXT.PixelTransform
import Graphics.GL.Ext.EXT.PixelTransformColorTable
import Graphics.GL.Ext.EXT.PointParameters
import Graphics.GL.Ext.EXT.PolygonOffset
import Graphics.GL.Ext.EXT.PolygonOffsetClamp
import Graphics.GL.Ext.EXT.PostDepthCoverage
import Graphics.GL.Ext.EXT.PrimitiveBoundingBox
import Graphics.GL.Ext.EXT.ProtectedTextures
import Graphics.GL.Ext.EXT.ProvokingVertex
import Graphics.GL.Ext.EXT.PvrtcSRGB
import Graphics.GL.Ext.EXT.RasterMultisample
import Graphics.GL.Ext.EXT.ReadFormatBgra
import Graphics.GL.Ext.EXT.RenderSnorm
import Graphics.GL.Ext.EXT.RescaleNormal
import Graphics.GL.Ext.EXT.Robustness
import Graphics.GL.Ext.EXT.SRGB
import Graphics.GL.Ext.EXT.SRGBWriteControl
import Graphics.GL.Ext.EXT.SecondaryColor
import Graphics.GL.Ext.EXT.SeparateShaderObjects
import Graphics.GL.Ext.EXT.SeparateSpecularColor
import Graphics.GL.Ext.EXT.ShaderFramebufferFetch
import Graphics.GL.Ext.EXT.ShaderGroupVote
import Graphics.GL.Ext.EXT.ShaderImageLoadFormatted
import Graphics.GL.Ext.EXT.ShaderImageLoadStore
import Graphics.GL.Ext.EXT.ShaderImplicitConversions
import Graphics.GL.Ext.EXT.ShaderIntegerMix
import Graphics.GL.Ext.EXT.ShaderIoBlocks
import Graphics.GL.Ext.EXT.ShaderNonConstantGlobalInitializers
import Graphics.GL.Ext.EXT.ShaderPixelLocalStorage
import Graphics.GL.Ext.EXT.ShaderPixelLocalStorage2
import Graphics.GL.Ext.EXT.ShaderTextureLod
import Graphics.GL.Ext.EXT.ShadowFuncs
import Graphics.GL.Ext.EXT.ShadowSamplers
import Graphics.GL.Ext.EXT.SharedTexturePalette
import Graphics.GL.Ext.EXT.SparseTexture
import Graphics.GL.Ext.EXT.SparseTexture2
import Graphics.GL.Ext.EXT.StencilClearTag
import Graphics.GL.Ext.EXT.StencilTwoSide
import Graphics.GL.Ext.EXT.StencilWrap
import Graphics.GL.Ext.EXT.Subtexture
import Graphics.GL.Ext.EXT.TessellationPointSize
import Graphics.GL.Ext.EXT.TessellationShader
import Graphics.GL.Ext.EXT.Texture
import Graphics.GL.Ext.EXT.Texture3D
import Graphics.GL.Ext.EXT.TextureArray
import Graphics.GL.Ext.EXT.TextureBorderClamp
import Graphics.GL.Ext.EXT.TextureBuffer
import Graphics.GL.Ext.EXT.TextureBufferObject
import Graphics.GL.Ext.EXT.TextureCompressionDxt1
import Graphics.GL.Ext.EXT.TextureCompressionLatc
import Graphics.GL.Ext.EXT.TextureCompressionRgtc
import Graphics.GL.Ext.EXT.TextureCompressionS3tc
import Graphics.GL.Ext.EXT.TextureCubeMap
import Graphics.GL.Ext.EXT.TextureCubeMapArray
import Graphics.GL.Ext.EXT.TextureEnvAdd
import Graphics.GL.Ext.EXT.TextureEnvCombine
import Graphics.GL.Ext.EXT.TextureEnvDot3
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic
import Graphics.GL.Ext.EXT.TextureFilterMinmax
import Graphics.GL.Ext.EXT.TextureFormatBGRA8888
import Graphics.GL.Ext.EXT.TextureInteger
import Graphics.GL.Ext.EXT.TextureLodBias
import Graphics.GL.Ext.EXT.TextureMirrorClamp
import Graphics.GL.Ext.EXT.TextureNorm16
import Graphics.GL.Ext.EXT.TextureObject
import Graphics.GL.Ext.EXT.TexturePerturbNormal
import Graphics.GL.Ext.EXT.TextureRg
import Graphics.GL.Ext.EXT.TextureSRGB
import Graphics.GL.Ext.EXT.TextureSRGBDecode
import Graphics.GL.Ext.EXT.TextureSRGBR8
import Graphics.GL.Ext.EXT.TextureSRGBRG8
import Graphics.GL.Ext.EXT.TextureSharedExponent
import Graphics.GL.Ext.EXT.TextureSnorm
import Graphics.GL.Ext.EXT.TextureStorage
import Graphics.GL.Ext.EXT.TextureSwizzle
import Graphics.GL.Ext.EXT.TextureType2101010REV
import Graphics.GL.Ext.EXT.TextureView
import Graphics.GL.Ext.EXT.TimerQuery
import Graphics.GL.Ext.EXT.TransformFeedback
import Graphics.GL.Ext.EXT.UnpackSubimage
import Graphics.GL.Ext.EXT.VertexArray
import Graphics.GL.Ext.EXT.VertexArrayBgra
import Graphics.GL.Ext.EXT.VertexAttrib64bit
import Graphics.GL.Ext.EXT.VertexShader
import Graphics.GL.Ext.EXT.VertexWeighting
import Graphics.GL.Ext.EXT.WindowRectangles
import Graphics.GL.Ext.EXT.X11SyncObject
import Graphics.GL.Ext.EXT.YUVTarget