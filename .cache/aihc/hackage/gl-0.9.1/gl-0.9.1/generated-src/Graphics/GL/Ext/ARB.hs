-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB (
  -- * ARB Extensions
    module Graphics.GL.Ext.ARB.ArraysOfArrays
  , module Graphics.GL.Ext.ARB.BaseInstance
  , module Graphics.GL.Ext.ARB.BindlessTexture
  , module Graphics.GL.Ext.ARB.BlendFuncExtended
  , module Graphics.GL.Ext.ARB.BufferStorage
  , module Graphics.GL.Ext.ARB.ClEvent
  , module Graphics.GL.Ext.ARB.ClearBufferObject
  , module Graphics.GL.Ext.ARB.ClearTexture
  , module Graphics.GL.Ext.ARB.ClipControl
  , module Graphics.GL.Ext.ARB.ColorBufferFloat
  , module Graphics.GL.Ext.ARB.Compatibility
  , module Graphics.GL.Ext.ARB.CompressedTexturePixelStorage
  , module Graphics.GL.Ext.ARB.ComputeShader
  , module Graphics.GL.Ext.ARB.ComputeVariableGroupSize
  , module Graphics.GL.Ext.ARB.ConditionalRenderInverted
  , module Graphics.GL.Ext.ARB.ConservativeDepth
  , module Graphics.GL.Ext.ARB.CopyBuffer
  , module Graphics.GL.Ext.ARB.CopyImage
  , module Graphics.GL.Ext.ARB.CullDistance
  , module Graphics.GL.Ext.ARB.DebugOutput
  , module Graphics.GL.Ext.ARB.DepthBufferFloat
  , module Graphics.GL.Ext.ARB.DepthClamp
  , module Graphics.GL.Ext.ARB.DepthTexture
  , module Graphics.GL.Ext.ARB.DerivativeControl
  , module Graphics.GL.Ext.ARB.DirectStateAccess
  , module Graphics.GL.Ext.ARB.DrawBuffers
  , module Graphics.GL.Ext.ARB.DrawBuffersBlend
  , module Graphics.GL.Ext.ARB.DrawElementsBaseVertex
  , module Graphics.GL.Ext.ARB.DrawIndirect
  , module Graphics.GL.Ext.ARB.DrawInstanced
  , module Graphics.GL.Ext.ARB.ES2Compatibility
  , module Graphics.GL.Ext.ARB.ES31Compatibility
  , module Graphics.GL.Ext.ARB.ES32Compatibility
  , module Graphics.GL.Ext.ARB.ES3Compatibility
  , module Graphics.GL.Ext.ARB.EnhancedLayouts
  , module Graphics.GL.Ext.ARB.ExplicitAttribLocation
  , module Graphics.GL.Ext.ARB.ExplicitUniformLocation
  , module Graphics.GL.Ext.ARB.FragmentCoordConventions
  , module Graphics.GL.Ext.ARB.FragmentLayerViewport
  , module Graphics.GL.Ext.ARB.FragmentProgram
  , module Graphics.GL.Ext.ARB.FragmentProgramShadow
  , module Graphics.GL.Ext.ARB.FragmentShader
  , module Graphics.GL.Ext.ARB.FragmentShaderInterlock
  , module Graphics.GL.Ext.ARB.FramebufferNoAttachments
  , module Graphics.GL.Ext.ARB.FramebufferObject
  , module Graphics.GL.Ext.ARB.FramebufferSRGB
  , module Graphics.GL.Ext.ARB.GeometryShader4
  , module Graphics.GL.Ext.ARB.GetProgramBinary
  , module Graphics.GL.Ext.ARB.GetTextureSubImage
  , module Graphics.GL.Ext.ARB.GpuShader5
  , module Graphics.GL.Ext.ARB.GpuShaderFp64
  , module Graphics.GL.Ext.ARB.GpuShaderInt64
  , module Graphics.GL.Ext.ARB.HalfFloatPixel
  , module Graphics.GL.Ext.ARB.HalfFloatVertex
  , module Graphics.GL.Ext.ARB.Imaging
  , module Graphics.GL.Ext.ARB.IndirectParameters
  , module Graphics.GL.Ext.ARB.InstancedArrays
  , module Graphics.GL.Ext.ARB.InternalformatQuery
  , module Graphics.GL.Ext.ARB.InternalformatQuery2
  , module Graphics.GL.Ext.ARB.InvalidateSubdata
  , module Graphics.GL.Ext.ARB.MapBufferAlignment
  , module Graphics.GL.Ext.ARB.MapBufferRange
  , module Graphics.GL.Ext.ARB.MatrixPalette
  , module Graphics.GL.Ext.ARB.MultiBind
  , module Graphics.GL.Ext.ARB.MultiDrawIndirect
  , module Graphics.GL.Ext.ARB.Multisample
  , module Graphics.GL.Ext.ARB.Multitexture
  , module Graphics.GL.Ext.ARB.OcclusionQuery
  , module Graphics.GL.Ext.ARB.OcclusionQuery2
  , module Graphics.GL.Ext.ARB.ParallelShaderCompile
  , module Graphics.GL.Ext.ARB.PipelineStatisticsQuery
  , module Graphics.GL.Ext.ARB.PixelBufferObject
  , module Graphics.GL.Ext.ARB.PointParameters
  , module Graphics.GL.Ext.ARB.PointSprite
  , module Graphics.GL.Ext.ARB.PostDepthCoverage
  , module Graphics.GL.Ext.ARB.ProgramInterfaceQuery
  , module Graphics.GL.Ext.ARB.ProvokingVertex
  , module Graphics.GL.Ext.ARB.QueryBufferObject
  , module Graphics.GL.Ext.ARB.RobustBufferAccessBehavior
  , module Graphics.GL.Ext.ARB.Robustness
  , module Graphics.GL.Ext.ARB.RobustnessIsolation
  , module Graphics.GL.Ext.ARB.SampleLocations
  , module Graphics.GL.Ext.ARB.SampleShading
  , module Graphics.GL.Ext.ARB.SamplerObjects
  , module Graphics.GL.Ext.ARB.SeamlessCubeMap
  , module Graphics.GL.Ext.ARB.SeamlessCubemapPerTexture
  , module Graphics.GL.Ext.ARB.SeparateShaderObjects
  , module Graphics.GL.Ext.ARB.ShaderAtomicCounterOps
  , module Graphics.GL.Ext.ARB.ShaderAtomicCounters
  , module Graphics.GL.Ext.ARB.ShaderBallot
  , module Graphics.GL.Ext.ARB.ShaderBitEncoding
  , module Graphics.GL.Ext.ARB.ShaderClock
  , module Graphics.GL.Ext.ARB.ShaderDrawParameters
  , module Graphics.GL.Ext.ARB.ShaderGroupVote
  , module Graphics.GL.Ext.ARB.ShaderImageLoadStore
  , module Graphics.GL.Ext.ARB.ShaderImageSize
  , module Graphics.GL.Ext.ARB.ShaderObjects
  , module Graphics.GL.Ext.ARB.ShaderPrecision
  , module Graphics.GL.Ext.ARB.ShaderStencilExport
  , module Graphics.GL.Ext.ARB.ShaderStorageBufferObject
  , module Graphics.GL.Ext.ARB.ShaderSubroutine
  , module Graphics.GL.Ext.ARB.ShaderTextureImageSamples
  , module Graphics.GL.Ext.ARB.ShaderTextureLod
  , module Graphics.GL.Ext.ARB.ShaderViewportLayerArray
  , module Graphics.GL.Ext.ARB.ShadingLanguage100
  , module Graphics.GL.Ext.ARB.ShadingLanguage420pack
  , module Graphics.GL.Ext.ARB.ShadingLanguageInclude
  , module Graphics.GL.Ext.ARB.ShadingLanguagePacking
  , module Graphics.GL.Ext.ARB.Shadow
  , module Graphics.GL.Ext.ARB.ShadowAmbient
  , module Graphics.GL.Ext.ARB.SparseBuffer
  , module Graphics.GL.Ext.ARB.SparseTexture
  , module Graphics.GL.Ext.ARB.SparseTexture2
  , module Graphics.GL.Ext.ARB.SparseTextureClamp
  , module Graphics.GL.Ext.ARB.StencilTexturing
  , module Graphics.GL.Ext.ARB.Sync
  , module Graphics.GL.Ext.ARB.TessellationShader
  , module Graphics.GL.Ext.ARB.TextureBarrier
  , module Graphics.GL.Ext.ARB.TextureBorderClamp
  , module Graphics.GL.Ext.ARB.TextureBufferObject
  , module Graphics.GL.Ext.ARB.TextureBufferObjectRgb32
  , module Graphics.GL.Ext.ARB.TextureBufferRange
  , module Graphics.GL.Ext.ARB.TextureCompression
  , module Graphics.GL.Ext.ARB.TextureCompressionBptc
  , module Graphics.GL.Ext.ARB.TextureCompressionRgtc
  , module Graphics.GL.Ext.ARB.TextureCubeMap
  , module Graphics.GL.Ext.ARB.TextureCubeMapArray
  , module Graphics.GL.Ext.ARB.TextureEnvAdd
  , module Graphics.GL.Ext.ARB.TextureEnvCombine
  , module Graphics.GL.Ext.ARB.TextureEnvCrossbar
  , module Graphics.GL.Ext.ARB.TextureEnvDot3
  , module Graphics.GL.Ext.ARB.TextureFilterMinmax
  , module Graphics.GL.Ext.ARB.TextureFloat
  , module Graphics.GL.Ext.ARB.TextureGather
  , module Graphics.GL.Ext.ARB.TextureMirrorClampToEdge
  , module Graphics.GL.Ext.ARB.TextureMirroredRepeat
  , module Graphics.GL.Ext.ARB.TextureMultisample
  , module Graphics.GL.Ext.ARB.TextureNonPowerOfTwo
  , module Graphics.GL.Ext.ARB.TextureQueryLevels
  , module Graphics.GL.Ext.ARB.TextureQueryLod
  , module Graphics.GL.Ext.ARB.TextureRectangle
  , module Graphics.GL.Ext.ARB.TextureRg
  , module Graphics.GL.Ext.ARB.TextureRgb10A2ui
  , module Graphics.GL.Ext.ARB.TextureStencil8
  , module Graphics.GL.Ext.ARB.TextureStorage
  , module Graphics.GL.Ext.ARB.TextureStorageMultisample
  , module Graphics.GL.Ext.ARB.TextureSwizzle
  , module Graphics.GL.Ext.ARB.TextureView
  , module Graphics.GL.Ext.ARB.TimerQuery
  , module Graphics.GL.Ext.ARB.TransformFeedback2
  , module Graphics.GL.Ext.ARB.TransformFeedback3
  , module Graphics.GL.Ext.ARB.TransformFeedbackInstanced
  , module Graphics.GL.Ext.ARB.TransformFeedbackOverflowQuery
  , module Graphics.GL.Ext.ARB.TransposeMatrix
  , module Graphics.GL.Ext.ARB.UniformBufferObject
  , module Graphics.GL.Ext.ARB.VertexArrayBgra
  , module Graphics.GL.Ext.ARB.VertexArrayObject
  , module Graphics.GL.Ext.ARB.VertexAttrib64bit
  , module Graphics.GL.Ext.ARB.VertexAttribBinding
  , module Graphics.GL.Ext.ARB.VertexBlend
  , module Graphics.GL.Ext.ARB.VertexBufferObject
  , module Graphics.GL.Ext.ARB.VertexProgram
  , module Graphics.GL.Ext.ARB.VertexShader
  , module Graphics.GL.Ext.ARB.VertexType10f11f11fRev
  , module Graphics.GL.Ext.ARB.VertexType2101010Rev
  , module Graphics.GL.Ext.ARB.ViewportArray
  , module Graphics.GL.Ext.ARB.WindowPos
) where

import Graphics.GL.Ext.ARB.ArraysOfArrays
import Graphics.GL.Ext.ARB.BaseInstance
import Graphics.GL.Ext.ARB.BindlessTexture
import Graphics.GL.Ext.ARB.BlendFuncExtended
import Graphics.GL.Ext.ARB.BufferStorage
import Graphics.GL.Ext.ARB.ClEvent
import Graphics.GL.Ext.ARB.ClearBufferObject
import Graphics.GL.Ext.ARB.ClearTexture
import Graphics.GL.Ext.ARB.ClipControl
import Graphics.GL.Ext.ARB.ColorBufferFloat
import Graphics.GL.Ext.ARB.Compatibility
import Graphics.GL.Ext.ARB.CompressedTexturePixelStorage
import Graphics.GL.Ext.ARB.ComputeShader
import Graphics.GL.Ext.ARB.ComputeVariableGroupSize
import Graphics.GL.Ext.ARB.ConditionalRenderInverted
import Graphics.GL.Ext.ARB.ConservativeDepth
import Graphics.GL.Ext.ARB.CopyBuffer
import Graphics.GL.Ext.ARB.CopyImage
import Graphics.GL.Ext.ARB.CullDistance
import Graphics.GL.Ext.ARB.DebugOutput
import Graphics.GL.Ext.ARB.DepthBufferFloat
import Graphics.GL.Ext.ARB.DepthClamp
import Graphics.GL.Ext.ARB.DepthTexture
import Graphics.GL.Ext.ARB.DerivativeControl
import Graphics.GL.Ext.ARB.DirectStateAccess
import Graphics.GL.Ext.ARB.DrawBuffers
import Graphics.GL.Ext.ARB.DrawBuffersBlend
import Graphics.GL.Ext.ARB.DrawElementsBaseVertex
import Graphics.GL.Ext.ARB.DrawIndirect
import Graphics.GL.Ext.ARB.DrawInstanced
import Graphics.GL.Ext.ARB.ES2Compatibility
import Graphics.GL.Ext.ARB.ES31Compatibility
import Graphics.GL.Ext.ARB.ES32Compatibility
import Graphics.GL.Ext.ARB.ES3Compatibility
import Graphics.GL.Ext.ARB.EnhancedLayouts
import Graphics.GL.Ext.ARB.ExplicitAttribLocation
import Graphics.GL.Ext.ARB.ExplicitUniformLocation
import Graphics.GL.Ext.ARB.FragmentCoordConventions
import Graphics.GL.Ext.ARB.FragmentLayerViewport
import Graphics.GL.Ext.ARB.FragmentProgram
import Graphics.GL.Ext.ARB.FragmentProgramShadow
import Graphics.GL.Ext.ARB.FragmentShader
import Graphics.GL.Ext.ARB.FragmentShaderInterlock
import Graphics.GL.Ext.ARB.FramebufferNoAttachments
import Graphics.GL.Ext.ARB.FramebufferObject
import Graphics.GL.Ext.ARB.FramebufferSRGB
import Graphics.GL.Ext.ARB.GeometryShader4
import Graphics.GL.Ext.ARB.GetProgramBinary
import Graphics.GL.Ext.ARB.GetTextureSubImage
import Graphics.GL.Ext.ARB.GpuShader5
import Graphics.GL.Ext.ARB.GpuShaderFp64
import Graphics.GL.Ext.ARB.GpuShaderInt64
import Graphics.GL.Ext.ARB.HalfFloatPixel
import Graphics.GL.Ext.ARB.HalfFloatVertex
import Graphics.GL.Ext.ARB.Imaging
import Graphics.GL.Ext.ARB.IndirectParameters
import Graphics.GL.Ext.ARB.InstancedArrays
import Graphics.GL.Ext.ARB.InternalformatQuery
import Graphics.GL.Ext.ARB.InternalformatQuery2
import Graphics.GL.Ext.ARB.InvalidateSubdata
import Graphics.GL.Ext.ARB.MapBufferAlignment
import Graphics.GL.Ext.ARB.MapBufferRange
import Graphics.GL.Ext.ARB.MatrixPalette
import Graphics.GL.Ext.ARB.MultiBind
import Graphics.GL.Ext.ARB.MultiDrawIndirect
import Graphics.GL.Ext.ARB.Multisample
import Graphics.GL.Ext.ARB.Multitexture
import Graphics.GL.Ext.ARB.OcclusionQuery
import Graphics.GL.Ext.ARB.OcclusionQuery2
import Graphics.GL.Ext.ARB.ParallelShaderCompile
import Graphics.GL.Ext.ARB.PipelineStatisticsQuery
import Graphics.GL.Ext.ARB.PixelBufferObject
import Graphics.GL.Ext.ARB.PointParameters
import Graphics.GL.Ext.ARB.PointSprite
import Graphics.GL.Ext.ARB.PostDepthCoverage
import Graphics.GL.Ext.ARB.ProgramInterfaceQuery
import Graphics.GL.Ext.ARB.ProvokingVertex
import Graphics.GL.Ext.ARB.QueryBufferObject
import Graphics.GL.Ext.ARB.RobustBufferAccessBehavior
import Graphics.GL.Ext.ARB.Robustness
import Graphics.GL.Ext.ARB.RobustnessIsolation
import Graphics.GL.Ext.ARB.SampleLocations
import Graphics.GL.Ext.ARB.SampleShading
import Graphics.GL.Ext.ARB.SamplerObjects
import Graphics.GL.Ext.ARB.SeamlessCubeMap
import Graphics.GL.Ext.ARB.SeamlessCubemapPerTexture
import Graphics.GL.Ext.ARB.SeparateShaderObjects
import Graphics.GL.Ext.ARB.ShaderAtomicCounterOps
import Graphics.GL.Ext.ARB.ShaderAtomicCounters
import Graphics.GL.Ext.ARB.ShaderBallot
import Graphics.GL.Ext.ARB.ShaderBitEncoding
import Graphics.GL.Ext.ARB.ShaderClock
import Graphics.GL.Ext.ARB.ShaderDrawParameters
import Graphics.GL.Ext.ARB.ShaderGroupVote
import Graphics.GL.Ext.ARB.ShaderImageLoadStore
import Graphics.GL.Ext.ARB.ShaderImageSize
import Graphics.GL.Ext.ARB.ShaderObjects
import Graphics.GL.Ext.ARB.ShaderPrecision
import Graphics.GL.Ext.ARB.ShaderStencilExport
import Graphics.GL.Ext.ARB.ShaderStorageBufferObject
import Graphics.GL.Ext.ARB.ShaderSubroutine
import Graphics.GL.Ext.ARB.ShaderTextureImageSamples
import Graphics.GL.Ext.ARB.ShaderTextureLod
import Graphics.GL.Ext.ARB.ShaderViewportLayerArray
import Graphics.GL.Ext.ARB.ShadingLanguage100
import Graphics.GL.Ext.ARB.ShadingLanguage420pack
import Graphics.GL.Ext.ARB.ShadingLanguageInclude
import Graphics.GL.Ext.ARB.ShadingLanguagePacking
import Graphics.GL.Ext.ARB.Shadow
import Graphics.GL.Ext.ARB.ShadowAmbient
import Graphics.GL.Ext.ARB.SparseBuffer
import Graphics.GL.Ext.ARB.SparseTexture
import Graphics.GL.Ext.ARB.SparseTexture2
import Graphics.GL.Ext.ARB.SparseTextureClamp
import Graphics.GL.Ext.ARB.StencilTexturing
import Graphics.GL.Ext.ARB.Sync
import Graphics.GL.Ext.ARB.TessellationShader
import Graphics.GL.Ext.ARB.TextureBarrier
import Graphics.GL.Ext.ARB.TextureBorderClamp
import Graphics.GL.Ext.ARB.TextureBufferObject
import Graphics.GL.Ext.ARB.TextureBufferObjectRgb32
import Graphics.GL.Ext.ARB.TextureBufferRange
import Graphics.GL.Ext.ARB.TextureCompression
import Graphics.GL.Ext.ARB.TextureCompressionBptc
import Graphics.GL.Ext.ARB.TextureCompressionRgtc
import Graphics.GL.Ext.ARB.TextureCubeMap
import Graphics.GL.Ext.ARB.TextureCubeMapArray
import Graphics.GL.Ext.ARB.TextureEnvAdd
import Graphics.GL.Ext.ARB.TextureEnvCombine
import Graphics.GL.Ext.ARB.TextureEnvCrossbar
import Graphics.GL.Ext.ARB.TextureEnvDot3
import Graphics.GL.Ext.ARB.TextureFilterMinmax
import Graphics.GL.Ext.ARB.TextureFloat
import Graphics.GL.Ext.ARB.TextureGather
import Graphics.GL.Ext.ARB.TextureMirrorClampToEdge
import Graphics.GL.Ext.ARB.TextureMirroredRepeat
import Graphics.GL.Ext.ARB.TextureMultisample
import Graphics.GL.Ext.ARB.TextureNonPowerOfTwo
import Graphics.GL.Ext.ARB.TextureQueryLevels
import Graphics.GL.Ext.ARB.TextureQueryLod
import Graphics.GL.Ext.ARB.TextureRectangle
import Graphics.GL.Ext.ARB.TextureRg
import Graphics.GL.Ext.ARB.TextureRgb10A2ui
import Graphics.GL.Ext.ARB.TextureStencil8
import Graphics.GL.Ext.ARB.TextureStorage
import Graphics.GL.Ext.ARB.TextureStorageMultisample
import Graphics.GL.Ext.ARB.TextureSwizzle
import Graphics.GL.Ext.ARB.TextureView
import Graphics.GL.Ext.ARB.TimerQuery
import Graphics.GL.Ext.ARB.TransformFeedback2
import Graphics.GL.Ext.ARB.TransformFeedback3
import Graphics.GL.Ext.ARB.TransformFeedbackInstanced
import Graphics.GL.Ext.ARB.TransformFeedbackOverflowQuery
import Graphics.GL.Ext.ARB.TransposeMatrix
import Graphics.GL.Ext.ARB.UniformBufferObject
import Graphics.GL.Ext.ARB.VertexArrayBgra
import Graphics.GL.Ext.ARB.VertexArrayObject
import Graphics.GL.Ext.ARB.VertexAttrib64bit
import Graphics.GL.Ext.ARB.VertexAttribBinding
import Graphics.GL.Ext.ARB.VertexBlend
import Graphics.GL.Ext.ARB.VertexBufferObject
import Graphics.GL.Ext.ARB.VertexProgram
import Graphics.GL.Ext.ARB.VertexShader
import Graphics.GL.Ext.ARB.VertexType10f11f11fRev
import Graphics.GL.Ext.ARB.VertexType2101010Rev
import Graphics.GL.Ext.ARB.ViewportArray
import Graphics.GL.Ext.ARB.WindowPos