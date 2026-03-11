-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV (
  -- * NV Extensions
    module Graphics.GL.Ext.NV.BindlessMultiDrawIndirect
  , module Graphics.GL.Ext.NV.BindlessMultiDrawIndirectCount
  , module Graphics.GL.Ext.NV.BindlessTexture
  , module Graphics.GL.Ext.NV.BlendEquationAdvanced
  , module Graphics.GL.Ext.NV.BlendEquationAdvancedCoherent
  , module Graphics.GL.Ext.NV.BlendSquare
  , module Graphics.GL.Ext.NV.ClipSpaceWScaling
  , module Graphics.GL.Ext.NV.CommandList
  , module Graphics.GL.Ext.NV.ComputeProgram5
  , module Graphics.GL.Ext.NV.ConditionalRender
  , module Graphics.GL.Ext.NV.ConservativeRaster
  , module Graphics.GL.Ext.NV.ConservativeRasterDilate
  , module Graphics.GL.Ext.NV.ConservativeRasterPreSnapTriangles
  , module Graphics.GL.Ext.NV.CopyBuffer
  , module Graphics.GL.Ext.NV.CopyDepthToColor
  , module Graphics.GL.Ext.NV.CopyImage
  , module Graphics.GL.Ext.NV.CoverageSample
  , module Graphics.GL.Ext.NV.DeepTexture3D
  , module Graphics.GL.Ext.NV.DepthBufferFloat
  , module Graphics.GL.Ext.NV.DepthClamp
  , module Graphics.GL.Ext.NV.DepthNonlinear
  , module Graphics.GL.Ext.NV.DrawBuffers
  , module Graphics.GL.Ext.NV.DrawInstanced
  , module Graphics.GL.Ext.NV.DrawTexture
  , module Graphics.GL.Ext.NV.Evaluators
  , module Graphics.GL.Ext.NV.ExplicitAttribLocation
  , module Graphics.GL.Ext.NV.ExplicitMultisample
  , module Graphics.GL.Ext.NV.FboColorAttachments
  , module Graphics.GL.Ext.NV.Fence
  , module Graphics.GL.Ext.NV.FillRectangle
  , module Graphics.GL.Ext.NV.FloatBuffer
  , module Graphics.GL.Ext.NV.FogDistance
  , module Graphics.GL.Ext.NV.FragmentCoverageToColor
  , module Graphics.GL.Ext.NV.FragmentProgram
  , module Graphics.GL.Ext.NV.FragmentProgram2
  , module Graphics.GL.Ext.NV.FragmentProgram4
  , module Graphics.GL.Ext.NV.FragmentProgramOption
  , module Graphics.GL.Ext.NV.FragmentShaderInterlock
  , module Graphics.GL.Ext.NV.FramebufferBlit
  , module Graphics.GL.Ext.NV.FramebufferMixedSamples
  , module Graphics.GL.Ext.NV.FramebufferMultisample
  , module Graphics.GL.Ext.NV.FramebufferMultisampleCoverage
  , module Graphics.GL.Ext.NV.GenerateMipmapSRGB
  , module Graphics.GL.Ext.NV.GeometryProgram4
  , module Graphics.GL.Ext.NV.GeometryShader4
  , module Graphics.GL.Ext.NV.GeometryShaderPassthrough
  , module Graphics.GL.Ext.NV.GpuProgram4
  , module Graphics.GL.Ext.NV.GpuProgram5
  , module Graphics.GL.Ext.NV.GpuProgram5MemExtended
  , module Graphics.GL.Ext.NV.GpuShader5
  , module Graphics.GL.Ext.NV.HalfFloat
  , module Graphics.GL.Ext.NV.ImageFormats
  , module Graphics.GL.Ext.NV.InstancedArrays
  , module Graphics.GL.Ext.NV.InternalformatSampleQuery
  , module Graphics.GL.Ext.NV.LightMaxExponent
  , module Graphics.GL.Ext.NV.MultisampleCoverage
  , module Graphics.GL.Ext.NV.MultisampleFilterHint
  , module Graphics.GL.Ext.NV.NonSquareMatrices
  , module Graphics.GL.Ext.NV.OcclusionQuery
  , module Graphics.GL.Ext.NV.PackedDepthStencil
  , module Graphics.GL.Ext.NV.ParameterBufferObject
  , module Graphics.GL.Ext.NV.ParameterBufferObject2
  , module Graphics.GL.Ext.NV.PathRendering
  , module Graphics.GL.Ext.NV.PathRenderingSharedEdge
  , module Graphics.GL.Ext.NV.PixelDataRange
  , module Graphics.GL.Ext.NV.PointSprite
  , module Graphics.GL.Ext.NV.PolygonMode
  , module Graphics.GL.Ext.NV.PresentVideo
  , module Graphics.GL.Ext.NV.PrimitiveRestart
  , module Graphics.GL.Ext.NV.ReadBuffer
  , module Graphics.GL.Ext.NV.ReadBufferFront
  , module Graphics.GL.Ext.NV.ReadDepth
  , module Graphics.GL.Ext.NV.ReadDepthStencil
  , module Graphics.GL.Ext.NV.ReadStencil
  , module Graphics.GL.Ext.NV.RegisterCombiners
  , module Graphics.GL.Ext.NV.RegisterCombiners2
  , module Graphics.GL.Ext.NV.RobustnessVideoMemoryPurge
  , module Graphics.GL.Ext.NV.SRGBFormats
  , module Graphics.GL.Ext.NV.SampleLocations
  , module Graphics.GL.Ext.NV.SampleMaskOverrideCoverage
  , module Graphics.GL.Ext.NV.ShaderAtomicCounters
  , module Graphics.GL.Ext.NV.ShaderAtomicFloat
  , module Graphics.GL.Ext.NV.ShaderAtomicFloat64
  , module Graphics.GL.Ext.NV.ShaderAtomicFp16Vector
  , module Graphics.GL.Ext.NV.ShaderAtomicInt64
  , module Graphics.GL.Ext.NV.ShaderBufferLoad
  , module Graphics.GL.Ext.NV.ShaderBufferStore
  , module Graphics.GL.Ext.NV.ShaderNoperspectiveInterpolation
  , module Graphics.GL.Ext.NV.ShaderStorageBufferObject
  , module Graphics.GL.Ext.NV.ShaderThreadGroup
  , module Graphics.GL.Ext.NV.ShaderThreadShuffle
  , module Graphics.GL.Ext.NV.ShadowSamplersArray
  , module Graphics.GL.Ext.NV.ShadowSamplersCube
  , module Graphics.GL.Ext.NV.StereoViewRendering
  , module Graphics.GL.Ext.NV.TessellationProgram5
  , module Graphics.GL.Ext.NV.TexgenEmboss
  , module Graphics.GL.Ext.NV.TexgenReflection
  , module Graphics.GL.Ext.NV.TextureBarrier
  , module Graphics.GL.Ext.NV.TextureBorderClamp
  , module Graphics.GL.Ext.NV.TextureCompressionS3tcUpdate
  , module Graphics.GL.Ext.NV.TextureCompressionVtc
  , module Graphics.GL.Ext.NV.TextureEnvCombine4
  , module Graphics.GL.Ext.NV.TextureExpandNormal
  , module Graphics.GL.Ext.NV.TextureMultisample
  , module Graphics.GL.Ext.NV.TextureNpot2DMipmap
  , module Graphics.GL.Ext.NV.TextureRectangle
  , module Graphics.GL.Ext.NV.TextureShader
  , module Graphics.GL.Ext.NV.TextureShader2
  , module Graphics.GL.Ext.NV.TextureShader3
  , module Graphics.GL.Ext.NV.TransformFeedback
  , module Graphics.GL.Ext.NV.TransformFeedback2
  , module Graphics.GL.Ext.NV.UniformBufferUnifiedMemory
  , module Graphics.GL.Ext.NV.VdpauInterop
  , module Graphics.GL.Ext.NV.VertexArrayRange
  , module Graphics.GL.Ext.NV.VertexArrayRange2
  , module Graphics.GL.Ext.NV.VertexAttribInteger64bit
  , module Graphics.GL.Ext.NV.VertexBufferUnifiedMemory
  , module Graphics.GL.Ext.NV.VertexProgram
  , module Graphics.GL.Ext.NV.VertexProgram11
  , module Graphics.GL.Ext.NV.VertexProgram2
  , module Graphics.GL.Ext.NV.VertexProgram2Option
  , module Graphics.GL.Ext.NV.VertexProgram3
  , module Graphics.GL.Ext.NV.VertexProgram4
  , module Graphics.GL.Ext.NV.VideoCapture
  , module Graphics.GL.Ext.NV.ViewportArray
  , module Graphics.GL.Ext.NV.ViewportArray2
  , module Graphics.GL.Ext.NV.ViewportSwizzle
) where

import Graphics.GL.Ext.NV.BindlessMultiDrawIndirect
import Graphics.GL.Ext.NV.BindlessMultiDrawIndirectCount
import Graphics.GL.Ext.NV.BindlessTexture
import Graphics.GL.Ext.NV.BlendEquationAdvanced
import Graphics.GL.Ext.NV.BlendEquationAdvancedCoherent
import Graphics.GL.Ext.NV.BlendSquare
import Graphics.GL.Ext.NV.ClipSpaceWScaling
import Graphics.GL.Ext.NV.CommandList
import Graphics.GL.Ext.NV.ComputeProgram5
import Graphics.GL.Ext.NV.ConditionalRender
import Graphics.GL.Ext.NV.ConservativeRaster
import Graphics.GL.Ext.NV.ConservativeRasterDilate
import Graphics.GL.Ext.NV.ConservativeRasterPreSnapTriangles
import Graphics.GL.Ext.NV.CopyBuffer
import Graphics.GL.Ext.NV.CopyDepthToColor
import Graphics.GL.Ext.NV.CopyImage
import Graphics.GL.Ext.NV.CoverageSample
import Graphics.GL.Ext.NV.DeepTexture3D
import Graphics.GL.Ext.NV.DepthBufferFloat
import Graphics.GL.Ext.NV.DepthClamp
import Graphics.GL.Ext.NV.DepthNonlinear
import Graphics.GL.Ext.NV.DrawBuffers
import Graphics.GL.Ext.NV.DrawInstanced
import Graphics.GL.Ext.NV.DrawTexture
import Graphics.GL.Ext.NV.Evaluators
import Graphics.GL.Ext.NV.ExplicitAttribLocation
import Graphics.GL.Ext.NV.ExplicitMultisample
import Graphics.GL.Ext.NV.FboColorAttachments
import Graphics.GL.Ext.NV.Fence
import Graphics.GL.Ext.NV.FillRectangle
import Graphics.GL.Ext.NV.FloatBuffer
import Graphics.GL.Ext.NV.FogDistance
import Graphics.GL.Ext.NV.FragmentCoverageToColor
import Graphics.GL.Ext.NV.FragmentProgram
import Graphics.GL.Ext.NV.FragmentProgram2
import Graphics.GL.Ext.NV.FragmentProgram4
import Graphics.GL.Ext.NV.FragmentProgramOption
import Graphics.GL.Ext.NV.FragmentShaderInterlock
import Graphics.GL.Ext.NV.FramebufferBlit
import Graphics.GL.Ext.NV.FramebufferMixedSamples
import Graphics.GL.Ext.NV.FramebufferMultisample
import Graphics.GL.Ext.NV.FramebufferMultisampleCoverage
import Graphics.GL.Ext.NV.GenerateMipmapSRGB
import Graphics.GL.Ext.NV.GeometryProgram4
import Graphics.GL.Ext.NV.GeometryShader4
import Graphics.GL.Ext.NV.GeometryShaderPassthrough
import Graphics.GL.Ext.NV.GpuProgram4
import Graphics.GL.Ext.NV.GpuProgram5
import Graphics.GL.Ext.NV.GpuProgram5MemExtended
import Graphics.GL.Ext.NV.GpuShader5
import Graphics.GL.Ext.NV.HalfFloat
import Graphics.GL.Ext.NV.ImageFormats
import Graphics.GL.Ext.NV.InstancedArrays
import Graphics.GL.Ext.NV.InternalformatSampleQuery
import Graphics.GL.Ext.NV.LightMaxExponent
import Graphics.GL.Ext.NV.MultisampleCoverage
import Graphics.GL.Ext.NV.MultisampleFilterHint
import Graphics.GL.Ext.NV.NonSquareMatrices
import Graphics.GL.Ext.NV.OcclusionQuery
import Graphics.GL.Ext.NV.PackedDepthStencil
import Graphics.GL.Ext.NV.ParameterBufferObject
import Graphics.GL.Ext.NV.ParameterBufferObject2
import Graphics.GL.Ext.NV.PathRendering
import Graphics.GL.Ext.NV.PathRenderingSharedEdge
import Graphics.GL.Ext.NV.PixelDataRange
import Graphics.GL.Ext.NV.PointSprite
import Graphics.GL.Ext.NV.PolygonMode
import Graphics.GL.Ext.NV.PresentVideo
import Graphics.GL.Ext.NV.PrimitiveRestart
import Graphics.GL.Ext.NV.ReadBuffer
import Graphics.GL.Ext.NV.ReadBufferFront
import Graphics.GL.Ext.NV.ReadDepth
import Graphics.GL.Ext.NV.ReadDepthStencil
import Graphics.GL.Ext.NV.ReadStencil
import Graphics.GL.Ext.NV.RegisterCombiners
import Graphics.GL.Ext.NV.RegisterCombiners2
import Graphics.GL.Ext.NV.RobustnessVideoMemoryPurge
import Graphics.GL.Ext.NV.SRGBFormats
import Graphics.GL.Ext.NV.SampleLocations
import Graphics.GL.Ext.NV.SampleMaskOverrideCoverage
import Graphics.GL.Ext.NV.ShaderAtomicCounters
import Graphics.GL.Ext.NV.ShaderAtomicFloat
import Graphics.GL.Ext.NV.ShaderAtomicFloat64
import Graphics.GL.Ext.NV.ShaderAtomicFp16Vector
import Graphics.GL.Ext.NV.ShaderAtomicInt64
import Graphics.GL.Ext.NV.ShaderBufferLoad
import Graphics.GL.Ext.NV.ShaderBufferStore
import Graphics.GL.Ext.NV.ShaderNoperspectiveInterpolation
import Graphics.GL.Ext.NV.ShaderStorageBufferObject
import Graphics.GL.Ext.NV.ShaderThreadGroup
import Graphics.GL.Ext.NV.ShaderThreadShuffle
import Graphics.GL.Ext.NV.ShadowSamplersArray
import Graphics.GL.Ext.NV.ShadowSamplersCube
import Graphics.GL.Ext.NV.StereoViewRendering
import Graphics.GL.Ext.NV.TessellationProgram5
import Graphics.GL.Ext.NV.TexgenEmboss
import Graphics.GL.Ext.NV.TexgenReflection
import Graphics.GL.Ext.NV.TextureBarrier
import Graphics.GL.Ext.NV.TextureBorderClamp
import Graphics.GL.Ext.NV.TextureCompressionS3tcUpdate
import Graphics.GL.Ext.NV.TextureCompressionVtc
import Graphics.GL.Ext.NV.TextureEnvCombine4
import Graphics.GL.Ext.NV.TextureExpandNormal
import Graphics.GL.Ext.NV.TextureMultisample
import Graphics.GL.Ext.NV.TextureNpot2DMipmap
import Graphics.GL.Ext.NV.TextureRectangle
import Graphics.GL.Ext.NV.TextureShader
import Graphics.GL.Ext.NV.TextureShader2
import Graphics.GL.Ext.NV.TextureShader3
import Graphics.GL.Ext.NV.TransformFeedback
import Graphics.GL.Ext.NV.TransformFeedback2
import Graphics.GL.Ext.NV.UniformBufferUnifiedMemory
import Graphics.GL.Ext.NV.VdpauInterop
import Graphics.GL.Ext.NV.VertexArrayRange
import Graphics.GL.Ext.NV.VertexArrayRange2
import Graphics.GL.Ext.NV.VertexAttribInteger64bit
import Graphics.GL.Ext.NV.VertexBufferUnifiedMemory
import Graphics.GL.Ext.NV.VertexProgram
import Graphics.GL.Ext.NV.VertexProgram11
import Graphics.GL.Ext.NV.VertexProgram2
import Graphics.GL.Ext.NV.VertexProgram2Option
import Graphics.GL.Ext.NV.VertexProgram3
import Graphics.GL.Ext.NV.VertexProgram4
import Graphics.GL.Ext.NV.VideoCapture
import Graphics.GL.Ext.NV.ViewportArray
import Graphics.GL.Ext.NV.ViewportArray2
import Graphics.GL.Ext.NV.ViewportSwizzle