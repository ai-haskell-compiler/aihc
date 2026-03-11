-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.VertexShader (
  -- * Extension Support
    gl_EXT_vertex_shader

  -- * GL_EXT_vertex_shader
  , glBeginVertexShaderEXT
  , glBindLightParameterEXT
  , glBindMaterialParameterEXT
  , glBindParameterEXT
  , glBindTexGenParameterEXT
  , glBindTextureUnitParameterEXT
  , glBindVertexShaderEXT
  , glDeleteVertexShaderEXT
  , glDisableVariantClientStateEXT
  , glEnableVariantClientStateEXT
  , glEndVertexShaderEXT
  , glExtractComponentEXT
  , glGenSymbolsEXT
  , glGenVertexShadersEXT
  , glGetInvariantBooleanvEXT
  , glGetInvariantFloatvEXT
  , glGetInvariantIntegervEXT
  , glGetLocalConstantBooleanvEXT
  , glGetLocalConstantFloatvEXT
  , glGetLocalConstantIntegervEXT
  , glGetVariantBooleanvEXT
  , glGetVariantFloatvEXT
  , glGetVariantIntegervEXT
  , glGetVariantPointervEXT
  , glInsertComponentEXT
  , glIsVariantEnabledEXT
  , glSetInvariantEXT
  , glSetLocalConstantEXT
  , glShaderOp1EXT
  , glShaderOp2EXT
  , glShaderOp3EXT
  , glSwizzleEXT
  , glVariantPointerEXT
  , glVariantbvEXT
  , glVariantdvEXT
  , glVariantfvEXT
  , glVariantivEXT
  , glVariantsvEXT
  , glVariantubvEXT
  , glVariantuivEXT
  , glVariantusvEXT
  , glWriteMaskEXT
  , pattern GL_CURRENT_VERTEX_EXT
  , pattern GL_FULL_RANGE_EXT
  , pattern GL_INVARIANT_DATATYPE_EXT
  , pattern GL_INVARIANT_EXT
  , pattern GL_INVARIANT_VALUE_EXT
  , pattern GL_LOCAL_CONSTANT_DATATYPE_EXT
  , pattern GL_LOCAL_CONSTANT_EXT
  , pattern GL_LOCAL_CONSTANT_VALUE_EXT
  , pattern GL_LOCAL_EXT
  , pattern GL_MATRIX_EXT
  , pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT
  , pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT
  , pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT
  , pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT
  , pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT
  , pattern GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT
  , pattern GL_MAX_VERTEX_SHADER_INVARIANTS_EXT
  , pattern GL_MAX_VERTEX_SHADER_LOCALS_EXT
  , pattern GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT
  , pattern GL_MAX_VERTEX_SHADER_VARIANTS_EXT
  , pattern GL_MVP_MATRIX_EXT
  , pattern GL_NEGATIVE_ONE_EXT
  , pattern GL_NEGATIVE_W_EXT
  , pattern GL_NEGATIVE_X_EXT
  , pattern GL_NEGATIVE_Y_EXT
  , pattern GL_NEGATIVE_Z_EXT
  , pattern GL_NORMALIZED_RANGE_EXT
  , pattern GL_ONE_EXT
  , pattern GL_OP_ADD_EXT
  , pattern GL_OP_CLAMP_EXT
  , pattern GL_OP_CROSS_PRODUCT_EXT
  , pattern GL_OP_DOT3_EXT
  , pattern GL_OP_DOT4_EXT
  , pattern GL_OP_EXP_BASE_2_EXT
  , pattern GL_OP_FLOOR_EXT
  , pattern GL_OP_FRAC_EXT
  , pattern GL_OP_INDEX_EXT
  , pattern GL_OP_LOG_BASE_2_EXT
  , pattern GL_OP_MADD_EXT
  , pattern GL_OP_MAX_EXT
  , pattern GL_OP_MIN_EXT
  , pattern GL_OP_MOV_EXT
  , pattern GL_OP_MULTIPLY_MATRIX_EXT
  , pattern GL_OP_MUL_EXT
  , pattern GL_OP_NEGATE_EXT
  , pattern GL_OP_POWER_EXT
  , pattern GL_OP_RECIP_EXT
  , pattern GL_OP_RECIP_SQRT_EXT
  , pattern GL_OP_ROUND_EXT
  , pattern GL_OP_SET_GE_EXT
  , pattern GL_OP_SET_LT_EXT
  , pattern GL_OP_SUB_EXT
  , pattern GL_OUTPUT_COLOR0_EXT
  , pattern GL_OUTPUT_COLOR1_EXT
  , pattern GL_OUTPUT_FOG_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD0_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD10_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD11_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD12_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD13_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD14_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD15_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD16_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD17_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD18_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD19_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD1_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD20_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD21_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD22_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD23_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD24_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD25_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD26_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD27_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD28_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD29_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD2_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD30_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD31_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD3_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD4_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD5_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD6_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD7_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD8_EXT
  , pattern GL_OUTPUT_TEXTURE_COORD9_EXT
  , pattern GL_OUTPUT_VERTEX_EXT
  , pattern GL_SCALAR_EXT
  , pattern GL_VARIANT_ARRAY_EXT
  , pattern GL_VARIANT_ARRAY_POINTER_EXT
  , pattern GL_VARIANT_ARRAY_STRIDE_EXT
  , pattern GL_VARIANT_ARRAY_TYPE_EXT
  , pattern GL_VARIANT_DATATYPE_EXT
  , pattern GL_VARIANT_EXT
  , pattern GL_VARIANT_VALUE_EXT
  , pattern GL_VECTOR_EXT
  , pattern GL_VERTEX_SHADER_BINDING_EXT
  , pattern GL_VERTEX_SHADER_EXT
  , pattern GL_VERTEX_SHADER_INSTRUCTIONS_EXT
  , pattern GL_VERTEX_SHADER_INVARIANTS_EXT
  , pattern GL_VERTEX_SHADER_LOCALS_EXT
  , pattern GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT
  , pattern GL_VERTEX_SHADER_OPTIMIZED_EXT
  , pattern GL_VERTEX_SHADER_VARIANTS_EXT
  , pattern GL_W_EXT
  , pattern GL_X_EXT
  , pattern GL_Y_EXT
  , pattern GL_ZERO_EXT
  , pattern GL_Z_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/vertex_shader.txt GL_EXT_vertex_shader> extension is available.

gl_EXT_vertex_shader :: Bool
gl_EXT_vertex_shader = member "GL_EXT_vertex_shader" extensions
{-# NOINLINE gl_EXT_vertex_shader #-}

-- | Usage: @'glBeginVertexShaderEXT'@


glBeginVertexShaderEXT :: MonadIO m => m ()
glBeginVertexShaderEXT = ffiIOV glBeginVertexShaderEXTFunPtr

glBeginVertexShaderEXTFunPtr :: FunPtr (IO ())
glBeginVertexShaderEXTFunPtr = unsafePerformIO (getProcAddress "glBeginVertexShaderEXT")

{-# NOINLINE glBeginVertexShaderEXTFunPtr #-}

-- | Usage: @'glBindLightParameterEXT' light value@
--
-- The parameter @light@ is a @LightName@, one of: 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT0_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT1_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT2_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT3_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT4_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT5_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT6_SGIX', 'Graphics.GL.Ext.SGIX.FragmentLighting.GL_FRAGMENT_LIGHT7_SGIX', 'Graphics.GL.Internal.Shared.GL_LIGHT0', 'Graphics.GL.Internal.Shared.GL_LIGHT1', 'Graphics.GL.Internal.Shared.GL_LIGHT2', 'Graphics.GL.Internal.Shared.GL_LIGHT3', 'Graphics.GL.Internal.Shared.GL_LIGHT4', 'Graphics.GL.Internal.Shared.GL_LIGHT5', 'Graphics.GL.Internal.Shared.GL_LIGHT6', 'Graphics.GL.Internal.Shared.GL_LIGHT7'.
--
-- The parameter @value@ is a @LightParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_CONSTANT_ATTENUATION', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_LINEAR_ATTENUATION', 'Graphics.GL.Internal.Shared.GL_POSITION', 'Graphics.GL.Internal.Shared.GL_QUADRATIC_ATTENUATION', 'Graphics.GL.Internal.Shared.GL_SPECULAR', 'Graphics.GL.Internal.Shared.GL_SPOT_CUTOFF', 'Graphics.GL.Internal.Shared.GL_SPOT_DIRECTION', 'Graphics.GL.Internal.Shared.GL_SPOT_EXPONENT'.


glBindLightParameterEXT :: MonadIO m => GLenum -> GLenum -> m GLuint
glBindLightParameterEXT = ffienumenumIOuint glBindLightParameterEXTFunPtr

glBindLightParameterEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO GLuint)
glBindLightParameterEXTFunPtr = unsafePerformIO (getProcAddress "glBindLightParameterEXT")

{-# NOINLINE glBindLightParameterEXTFunPtr #-}

-- | Usage: @'glBindMaterialParameterEXT' face value@
--
-- The parameter @face@ is a @MaterialFace@, one of: 'Graphics.GL.Internal.Shared.GL_BACK', 'Graphics.GL.Internal.Shared.GL_FRONT', 'Graphics.GL.Internal.Shared.GL_FRONT_AND_BACK'.
--
-- The parameter @value@ is a @MaterialParameter@, one of: 'Graphics.GL.Internal.Shared.GL_AMBIENT', 'Graphics.GL.Internal.Shared.GL_AMBIENT_AND_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_COLOR_INDEXES', 'Graphics.GL.Internal.Shared.GL_DIFFUSE', 'Graphics.GL.Internal.Shared.GL_EMISSION', 'Graphics.GL.Internal.Shared.GL_SHININESS', 'Graphics.GL.Internal.Shared.GL_SPECULAR'.


glBindMaterialParameterEXT :: MonadIO m => GLenum -> GLenum -> m GLuint
glBindMaterialParameterEXT = ffienumenumIOuint glBindMaterialParameterEXTFunPtr

glBindMaterialParameterEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO GLuint)
glBindMaterialParameterEXTFunPtr = unsafePerformIO (getProcAddress "glBindMaterialParameterEXT")

{-# NOINLINE glBindMaterialParameterEXTFunPtr #-}

-- | Usage: @'glBindParameterEXT' value@
--
-- The parameter @value@ is a @VertexShaderParameterEXT@.


glBindParameterEXT :: MonadIO m => GLenum -> m GLuint
glBindParameterEXT = ffienumIOuint glBindParameterEXTFunPtr

glBindParameterEXTFunPtr :: FunPtr (GLenum -> IO GLuint)
glBindParameterEXTFunPtr = unsafePerformIO (getProcAddress "glBindParameterEXT")

{-# NOINLINE glBindParameterEXTFunPtr #-}

-- | Usage: @'glBindTexGenParameterEXT' unit coord value@
--
-- The parameter @unit@ is a @TextureUnit@.
--
-- The parameter @coord@ is a @TextureCoordName@, one of: 'Graphics.GL.Internal.Shared.GL_S', 'Graphics.GL.Internal.Shared.GL_T', 'Graphics.GL.Internal.Shared.GL_R', 'Graphics.GL.Internal.Shared.GL_Q'.
--
-- The parameter @value@ is a @TextureGenParameter@, one of: 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_EYE_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_EYE_POINT_SGIS', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_LINE_SGIS', 'Graphics.GL.Internal.Shared.GL_OBJECT_PLANE', 'Graphics.GL.Ext.SGIS.PointLineTexgen.GL_OBJECT_POINT_SGIS', 'Graphics.GL.Internal.Shared.GL_TEXTURE_GEN_MODE'.


glBindTexGenParameterEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> m GLuint
glBindTexGenParameterEXT = ffienumenumenumIOuint glBindTexGenParameterEXTFunPtr

glBindTexGenParameterEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> IO GLuint)
glBindTexGenParameterEXTFunPtr = unsafePerformIO (getProcAddress "glBindTexGenParameterEXT")

{-# NOINLINE glBindTexGenParameterEXTFunPtr #-}

-- | Usage: @'glBindTextureUnitParameterEXT' unit value@
--
-- The parameter @unit@ is a @TextureUnit@.
--
-- The parameter @value@ is a @VertexShaderTextureUnitParameter@.


glBindTextureUnitParameterEXT :: MonadIO m => GLenum -> GLenum -> m GLuint
glBindTextureUnitParameterEXT = ffienumenumIOuint glBindTextureUnitParameterEXTFunPtr

glBindTextureUnitParameterEXTFunPtr :: FunPtr (GLenum -> GLenum -> IO GLuint)
glBindTextureUnitParameterEXTFunPtr = unsafePerformIO (getProcAddress "glBindTextureUnitParameterEXT")

{-# NOINLINE glBindTextureUnitParameterEXTFunPtr #-}

-- | Usage: @'glBindVertexShaderEXT' id@


glBindVertexShaderEXT :: MonadIO m => GLuint -> m ()
glBindVertexShaderEXT = ffiuintIOV glBindVertexShaderEXTFunPtr

glBindVertexShaderEXTFunPtr :: FunPtr (GLuint -> IO ())
glBindVertexShaderEXTFunPtr = unsafePerformIO (getProcAddress "glBindVertexShaderEXT")

{-# NOINLINE glBindVertexShaderEXTFunPtr #-}

-- | Usage: @'glDeleteVertexShaderEXT' id@


glDeleteVertexShaderEXT :: MonadIO m => GLuint -> m ()
glDeleteVertexShaderEXT = ffiuintIOV glDeleteVertexShaderEXTFunPtr

glDeleteVertexShaderEXTFunPtr :: FunPtr (GLuint -> IO ())
glDeleteVertexShaderEXTFunPtr = unsafePerformIO (getProcAddress "glDeleteVertexShaderEXT")

{-# NOINLINE glDeleteVertexShaderEXTFunPtr #-}

-- | Usage: @'glDisableVariantClientStateEXT' id@


glDisableVariantClientStateEXT :: MonadIO m => GLuint -> m ()
glDisableVariantClientStateEXT = ffiuintIOV glDisableVariantClientStateEXTFunPtr

glDisableVariantClientStateEXTFunPtr :: FunPtr (GLuint -> IO ())
glDisableVariantClientStateEXTFunPtr = unsafePerformIO (getProcAddress "glDisableVariantClientStateEXT")

{-# NOINLINE glDisableVariantClientStateEXTFunPtr #-}

-- | Usage: @'glEnableVariantClientStateEXT' id@


glEnableVariantClientStateEXT :: MonadIO m => GLuint -> m ()
glEnableVariantClientStateEXT = ffiuintIOV glEnableVariantClientStateEXTFunPtr

glEnableVariantClientStateEXTFunPtr :: FunPtr (GLuint -> IO ())
glEnableVariantClientStateEXTFunPtr = unsafePerformIO (getProcAddress "glEnableVariantClientStateEXT")

{-# NOINLINE glEnableVariantClientStateEXTFunPtr #-}

-- | Usage: @'glEndVertexShaderEXT'@


glEndVertexShaderEXT :: MonadIO m => m ()
glEndVertexShaderEXT = ffiIOV glEndVertexShaderEXTFunPtr

glEndVertexShaderEXTFunPtr :: FunPtr (IO ())
glEndVertexShaderEXTFunPtr = unsafePerformIO (getProcAddress "glEndVertexShaderEXT")

{-# NOINLINE glEndVertexShaderEXTFunPtr #-}

-- | Usage: @'glExtractComponentEXT' res src num@


glExtractComponentEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glExtractComponentEXT = ffiuintuintuintIOV glExtractComponentEXTFunPtr

glExtractComponentEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glExtractComponentEXTFunPtr = unsafePerformIO (getProcAddress "glExtractComponentEXT")

{-# NOINLINE glExtractComponentEXTFunPtr #-}

-- | Usage: @'glGenSymbolsEXT' datatype storagetype range components@
--
-- The parameter @datatype@ is a @DataTypeEXT@.
--
-- The parameter @storagetype@ is a @VertexShaderStorageTypeEXT@.
--
-- The parameter @range@ is a @ParameterRangeEXT@.


glGenSymbolsEXT :: MonadIO m => GLenum -> GLenum -> GLenum -> GLuint -> m GLuint
glGenSymbolsEXT = ffienumenumenumuintIOuint glGenSymbolsEXTFunPtr

glGenSymbolsEXTFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO GLuint)
glGenSymbolsEXTFunPtr = unsafePerformIO (getProcAddress "glGenSymbolsEXT")

{-# NOINLINE glGenSymbolsEXTFunPtr #-}

-- | Usage: @'glGenVertexShadersEXT' range@


glGenVertexShadersEXT :: MonadIO m => GLuint -> m GLuint
glGenVertexShadersEXT = ffiuintIOuint glGenVertexShadersEXTFunPtr

glGenVertexShadersEXTFunPtr :: FunPtr (GLuint -> IO GLuint)
glGenVertexShadersEXTFunPtr = unsafePerformIO (getProcAddress "glGenVertexShadersEXT")

{-# NOINLINE glGenVertexShadersEXTFunPtr #-}

-- | Usage: @'glGetInvariantBooleanvEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The parameter @data@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetInvariantBooleanvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLboolean -> m ()
glGetInvariantBooleanvEXT = ffiuintenumPtrbooleanIOV glGetInvariantBooleanvEXTFunPtr

glGetInvariantBooleanvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
glGetInvariantBooleanvEXTFunPtr = unsafePerformIO (getProcAddress "glGetInvariantBooleanvEXT")

{-# NOINLINE glGetInvariantBooleanvEXTFunPtr #-}

-- | Usage: @'glGetInvariantFloatvEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetInvariantFloatvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetInvariantFloatvEXT = ffiuintenumPtrfloatIOV glGetInvariantFloatvEXTFunPtr

glGetInvariantFloatvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetInvariantFloatvEXTFunPtr = unsafePerformIO (getProcAddress "glGetInvariantFloatvEXT")

{-# NOINLINE glGetInvariantFloatvEXTFunPtr #-}

-- | Usage: @'glGetInvariantIntegervEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetInvariantIntegervEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetInvariantIntegervEXT = ffiuintenumPtrintIOV glGetInvariantIntegervEXTFunPtr

glGetInvariantIntegervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetInvariantIntegervEXTFunPtr = unsafePerformIO (getProcAddress "glGetInvariantIntegervEXT")

{-# NOINLINE glGetInvariantIntegervEXTFunPtr #-}

-- | Usage: @'glGetLocalConstantBooleanvEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The parameter @data@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetLocalConstantBooleanvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLboolean -> m ()
glGetLocalConstantBooleanvEXT = ffiuintenumPtrbooleanIOV glGetLocalConstantBooleanvEXTFunPtr

glGetLocalConstantBooleanvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
glGetLocalConstantBooleanvEXTFunPtr = unsafePerformIO (getProcAddress "glGetLocalConstantBooleanvEXT")

{-# NOINLINE glGetLocalConstantBooleanvEXTFunPtr #-}

-- | Usage: @'glGetLocalConstantFloatvEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetLocalConstantFloatvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetLocalConstantFloatvEXT = ffiuintenumPtrfloatIOV glGetLocalConstantFloatvEXTFunPtr

glGetLocalConstantFloatvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetLocalConstantFloatvEXTFunPtr = unsafePerformIO (getProcAddress "glGetLocalConstantFloatvEXT")

{-# NOINLINE glGetLocalConstantFloatvEXTFunPtr #-}

-- | Usage: @'glGetLocalConstantIntegervEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetLocalConstantIntegervEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetLocalConstantIntegervEXT = ffiuintenumPtrintIOV glGetLocalConstantIntegervEXTFunPtr

glGetLocalConstantIntegervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetLocalConstantIntegervEXTFunPtr = unsafePerformIO (getProcAddress "glGetLocalConstantIntegervEXT")

{-# NOINLINE glGetLocalConstantIntegervEXTFunPtr #-}

-- | Usage: @'glGetVariantBooleanvEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The parameter @data@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetVariantBooleanvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLboolean -> m ()
glGetVariantBooleanvEXT = ffiuintenumPtrbooleanIOV glGetVariantBooleanvEXTFunPtr

glGetVariantBooleanvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
glGetVariantBooleanvEXTFunPtr = unsafePerformIO (getProcAddress "glGetVariantBooleanvEXT")

{-# NOINLINE glGetVariantBooleanvEXTFunPtr #-}

-- | Usage: @'glGetVariantFloatvEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetVariantFloatvEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetVariantFloatvEXT = ffiuintenumPtrfloatIOV glGetVariantFloatvEXTFunPtr

glGetVariantFloatvEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetVariantFloatvEXTFunPtr = unsafePerformIO (getProcAddress "glGetVariantFloatvEXT")

{-# NOINLINE glGetVariantFloatvEXTFunPtr #-}

-- | Usage: @'glGetVariantIntegervEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetVariantIntegervEXT :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetVariantIntegervEXT = ffiuintenumPtrintIOV glGetVariantIntegervEXTFunPtr

glGetVariantIntegervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetVariantIntegervEXTFunPtr = unsafePerformIO (getProcAddress "glGetVariantIntegervEXT")

{-# NOINLINE glGetVariantIntegervEXTFunPtr #-}

-- | Usage: @'glGetVariantPointervEXT' id value data@
--
-- The parameter @value@ is a @GetVariantValueEXT@.
--
-- The length of @data@ should be @COMPSIZE(id)@.


glGetVariantPointervEXT :: MonadIO m => GLuint -> GLenum -> Ptr (Ptr ()) -> m ()
glGetVariantPointervEXT = ffiuintenumPtrPtrVIOV glGetVariantPointervEXTFunPtr

glGetVariantPointervEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
glGetVariantPointervEXTFunPtr = unsafePerformIO (getProcAddress "glGetVariantPointervEXT")

{-# NOINLINE glGetVariantPointervEXTFunPtr #-}

-- | Usage: @'glInsertComponentEXT' res src num@


glInsertComponentEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glInsertComponentEXT = ffiuintuintuintIOV glInsertComponentEXTFunPtr

glInsertComponentEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glInsertComponentEXTFunPtr = unsafePerformIO (getProcAddress "glInsertComponentEXT")

{-# NOINLINE glInsertComponentEXTFunPtr #-}

-- | Usage: @'glIsVariantEnabledEXT' id cap@
--
-- The parameter @cap@ is a @VariantCapEXT@.


glIsVariantEnabledEXT :: MonadIO m => GLuint -> GLenum -> m GLboolean
glIsVariantEnabledEXT = ffiuintenumIOboolean glIsVariantEnabledEXTFunPtr

glIsVariantEnabledEXTFunPtr :: FunPtr (GLuint -> GLenum -> IO GLboolean)
glIsVariantEnabledEXTFunPtr = unsafePerformIO (getProcAddress "glIsVariantEnabledEXT")

{-# NOINLINE glIsVariantEnabledEXTFunPtr #-}

-- | Usage: @'glSetInvariantEXT' id type addr@
--
-- The parameter @type@ is a @ScalarType@.
--
-- The length of @addr@ should be @COMPSIZE(id,type)@.


glSetInvariantEXT :: MonadIO m => GLuint -> GLenum -> Ptr () -> m ()
glSetInvariantEXT = ffiuintenumPtrVIOV glSetInvariantEXTFunPtr

glSetInvariantEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr () -> IO ())
glSetInvariantEXTFunPtr = unsafePerformIO (getProcAddress "glSetInvariantEXT")

{-# NOINLINE glSetInvariantEXTFunPtr #-}

-- | Usage: @'glSetLocalConstantEXT' id type addr@
--
-- The parameter @type@ is a @ScalarType@.
--
-- The length of @addr@ should be @COMPSIZE(id,type)@.


glSetLocalConstantEXT :: MonadIO m => GLuint -> GLenum -> Ptr () -> m ()
glSetLocalConstantEXT = ffiuintenumPtrVIOV glSetLocalConstantEXTFunPtr

glSetLocalConstantEXTFunPtr :: FunPtr (GLuint -> GLenum -> Ptr () -> IO ())
glSetLocalConstantEXTFunPtr = unsafePerformIO (getProcAddress "glSetLocalConstantEXT")

{-# NOINLINE glSetLocalConstantEXTFunPtr #-}

-- | Usage: @'glShaderOp1EXT' op res arg1@
--
-- The parameter @op@ is a @VertexShaderOpEXT@.


glShaderOp1EXT :: MonadIO m => GLenum -> GLuint -> GLuint -> m ()
glShaderOp1EXT = ffienumuintuintIOV glShaderOp1EXTFunPtr

glShaderOp1EXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
glShaderOp1EXTFunPtr = unsafePerformIO (getProcAddress "glShaderOp1EXT")

{-# NOINLINE glShaderOp1EXTFunPtr #-}

-- | Usage: @'glShaderOp2EXT' op res arg1 arg2@
--
-- The parameter @op@ is a @VertexShaderOpEXT@.


glShaderOp2EXT :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> m ()
glShaderOp2EXT = ffienumuintuintuintIOV glShaderOp2EXTFunPtr

glShaderOp2EXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> IO ())
glShaderOp2EXTFunPtr = unsafePerformIO (getProcAddress "glShaderOp2EXT")

{-# NOINLINE glShaderOp2EXTFunPtr #-}

-- | Usage: @'glShaderOp3EXT' op res arg1 arg2 arg3@
--
-- The parameter @op@ is a @VertexShaderOpEXT@.


glShaderOp3EXT :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glShaderOp3EXT = ffienumuintuintuintuintIOV glShaderOp3EXTFunPtr

glShaderOp3EXTFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glShaderOp3EXTFunPtr = unsafePerformIO (getProcAddress "glShaderOp3EXT")

{-# NOINLINE glShaderOp3EXTFunPtr #-}

-- | Usage: @'glSwizzleEXT' res in outX outY outZ outW@
--
-- The parameter @outX@ is a @VertexShaderCoordOutEXT@.
--
-- The parameter @outY@ is a @VertexShaderCoordOutEXT@.
--
-- The parameter @outZ@ is a @VertexShaderCoordOutEXT@.
--
-- The parameter @outW@ is a @VertexShaderCoordOutEXT@.


glSwizzleEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glSwizzleEXT = ffiuintuintenumenumenumenumIOV glSwizzleEXTFunPtr

glSwizzleEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glSwizzleEXTFunPtr = unsafePerformIO (getProcAddress "glSwizzleEXT")

{-# NOINLINE glSwizzleEXTFunPtr #-}

-- | Usage: @'glVariantPointerEXT' id type stride addr@
--
-- The parameter @type@ is a @ScalarType@.
--
-- The length of @addr@ should be @COMPSIZE(id,type,stride)@.


glVariantPointerEXT :: MonadIO m => GLuint -> GLenum -> GLuint -> Ptr () -> m ()
glVariantPointerEXT = ffiuintenumuintPtrVIOV glVariantPointerEXTFunPtr

glVariantPointerEXTFunPtr :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr () -> IO ())
glVariantPointerEXTFunPtr = unsafePerformIO (getProcAddress "glVariantPointerEXT")

{-# NOINLINE glVariantPointerEXTFunPtr #-}

-- | Usage: @'glVariantbvEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantbvEXT :: MonadIO m => GLuint -> Ptr GLbyte -> m ()
glVariantbvEXT = ffiuintPtrbyteIOV glVariantbvEXTFunPtr

glVariantbvEXTFunPtr :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
glVariantbvEXTFunPtr = unsafePerformIO (getProcAddress "glVariantbvEXT")

{-# NOINLINE glVariantbvEXTFunPtr #-}

-- | Usage: @'glVariantdvEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantdvEXT :: MonadIO m => GLuint -> Ptr GLdouble -> m ()
glVariantdvEXT = ffiuintPtrdoubleIOV glVariantdvEXTFunPtr

glVariantdvEXTFunPtr :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
glVariantdvEXTFunPtr = unsafePerformIO (getProcAddress "glVariantdvEXT")

{-# NOINLINE glVariantdvEXTFunPtr #-}

-- | Usage: @'glVariantfvEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantfvEXT :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glVariantfvEXT = ffiuintPtrfloatIOV glVariantfvEXTFunPtr

glVariantfvEXTFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glVariantfvEXTFunPtr = unsafePerformIO (getProcAddress "glVariantfvEXT")

{-# NOINLINE glVariantfvEXTFunPtr #-}

-- | Usage: @'glVariantivEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantivEXT :: MonadIO m => GLuint -> Ptr GLint -> m ()
glVariantivEXT = ffiuintPtrintIOV glVariantivEXTFunPtr

glVariantivEXTFunPtr :: FunPtr (GLuint -> Ptr GLint -> IO ())
glVariantivEXTFunPtr = unsafePerformIO (getProcAddress "glVariantivEXT")

{-# NOINLINE glVariantivEXTFunPtr #-}

-- | Usage: @'glVariantsvEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantsvEXT :: MonadIO m => GLuint -> Ptr GLshort -> m ()
glVariantsvEXT = ffiuintPtrshortIOV glVariantsvEXTFunPtr

glVariantsvEXTFunPtr :: FunPtr (GLuint -> Ptr GLshort -> IO ())
glVariantsvEXTFunPtr = unsafePerformIO (getProcAddress "glVariantsvEXT")

{-# NOINLINE glVariantsvEXTFunPtr #-}

-- | Usage: @'glVariantubvEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantubvEXT :: MonadIO m => GLuint -> Ptr GLubyte -> m ()
glVariantubvEXT = ffiuintPtrubyteIOV glVariantubvEXTFunPtr

glVariantubvEXTFunPtr :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
glVariantubvEXTFunPtr = unsafePerformIO (getProcAddress "glVariantubvEXT")

{-# NOINLINE glVariantubvEXTFunPtr #-}

-- | Usage: @'glVariantuivEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantuivEXT :: MonadIO m => GLuint -> Ptr GLuint -> m ()
glVariantuivEXT = ffiuintPtruintIOV glVariantuivEXTFunPtr

glVariantuivEXTFunPtr :: FunPtr (GLuint -> Ptr GLuint -> IO ())
glVariantuivEXTFunPtr = unsafePerformIO (getProcAddress "glVariantuivEXT")

{-# NOINLINE glVariantuivEXTFunPtr #-}

-- | Usage: @'glVariantusvEXT' id addr@
--
-- The length of @addr@ should be @COMPSIZE(id)@.


glVariantusvEXT :: MonadIO m => GLuint -> Ptr GLushort -> m ()
glVariantusvEXT = ffiuintPtrushortIOV glVariantusvEXTFunPtr

glVariantusvEXTFunPtr :: FunPtr (GLuint -> Ptr GLushort -> IO ())
glVariantusvEXTFunPtr = unsafePerformIO (getProcAddress "glVariantusvEXT")

{-# NOINLINE glVariantusvEXTFunPtr #-}

-- | Usage: @'glWriteMaskEXT' res in outX outY outZ outW@
--
-- The parameter @outX@ is a @VertexShaderWriteMaskEXT@.
--
-- The parameter @outY@ is a @VertexShaderWriteMaskEXT@.
--
-- The parameter @outZ@ is a @VertexShaderWriteMaskEXT@.
--
-- The parameter @outW@ is a @VertexShaderWriteMaskEXT@.


glWriteMaskEXT :: MonadIO m => GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glWriteMaskEXT = ffiuintuintenumenumenumenumIOV glWriteMaskEXTFunPtr

glWriteMaskEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glWriteMaskEXTFunPtr = unsafePerformIO (getProcAddress "glWriteMaskEXT")

{-# NOINLINE glWriteMaskEXTFunPtr #-}

pattern GL_CURRENT_VERTEX_EXT  :: (Eq a, Num a) => a

pattern GL_CURRENT_VERTEX_EXT = 0x87E2

pattern GL_FULL_RANGE_EXT  :: (Eq a, Num a) => a

pattern GL_FULL_RANGE_EXT = 0x87E1

pattern GL_INVARIANT_DATATYPE_EXT  :: (Eq a, Num a) => a

pattern GL_INVARIANT_DATATYPE_EXT = 0x87EB

pattern GL_INVARIANT_EXT  :: (Eq a, Num a) => a

pattern GL_INVARIANT_EXT = 0x87C2

pattern GL_INVARIANT_VALUE_EXT  :: (Eq a, Num a) => a

pattern GL_INVARIANT_VALUE_EXT = 0x87EA

pattern GL_LOCAL_CONSTANT_DATATYPE_EXT  :: (Eq a, Num a) => a

pattern GL_LOCAL_CONSTANT_DATATYPE_EXT = 0x87ED

pattern GL_LOCAL_CONSTANT_EXT  :: (Eq a, Num a) => a

pattern GL_LOCAL_CONSTANT_EXT = 0x87C3

pattern GL_LOCAL_CONSTANT_VALUE_EXT  :: (Eq a, Num a) => a

pattern GL_LOCAL_CONSTANT_VALUE_EXT = 0x87EC

pattern GL_LOCAL_EXT  :: (Eq a, Num a) => a

pattern GL_LOCAL_EXT = 0x87C4

pattern GL_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_MATRIX_EXT = 0x87C0

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT = 0x87CA

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT = 0x87CD

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT = 0x87CE

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = 0x87CC

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT = 0x87CB

pattern GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT = 0x87C5

pattern GL_MAX_VERTEX_SHADER_INVARIANTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_SHADER_INVARIANTS_EXT = 0x87C7

pattern GL_MAX_VERTEX_SHADER_LOCALS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_SHADER_LOCALS_EXT = 0x87C9

pattern GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = 0x87C8

pattern GL_MAX_VERTEX_SHADER_VARIANTS_EXT  :: (Eq a, Num a) => a

pattern GL_MAX_VERTEX_SHADER_VARIANTS_EXT = 0x87C6

pattern GL_MVP_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_MVP_MATRIX_EXT = 0x87E3

pattern GL_NEGATIVE_ONE_EXT  :: (Eq a, Num a) => a

pattern GL_NEGATIVE_ONE_EXT = 0x87DF

pattern GL_NEGATIVE_W_EXT  :: (Eq a, Num a) => a

pattern GL_NEGATIVE_W_EXT = 0x87DC

pattern GL_NEGATIVE_X_EXT  :: (Eq a, Num a) => a

pattern GL_NEGATIVE_X_EXT = 0x87D9

pattern GL_NEGATIVE_Y_EXT  :: (Eq a, Num a) => a

pattern GL_NEGATIVE_Y_EXT = 0x87DA

pattern GL_NEGATIVE_Z_EXT  :: (Eq a, Num a) => a

pattern GL_NEGATIVE_Z_EXT = 0x87DB

pattern GL_NORMALIZED_RANGE_EXT  :: (Eq a, Num a) => a

pattern GL_NORMALIZED_RANGE_EXT = 0x87E0

pattern GL_ONE_EXT  :: (Eq a, Num a) => a

pattern GL_ONE_EXT = 0x87DE

pattern GL_OP_ADD_EXT  :: (Eq a, Num a) => a

pattern GL_OP_ADD_EXT = 0x8787

pattern GL_OP_CLAMP_EXT  :: (Eq a, Num a) => a

pattern GL_OP_CLAMP_EXT = 0x878E

pattern GL_OP_CROSS_PRODUCT_EXT  :: (Eq a, Num a) => a

pattern GL_OP_CROSS_PRODUCT_EXT = 0x8797

pattern GL_OP_DOT3_EXT  :: (Eq a, Num a) => a

pattern GL_OP_DOT3_EXT = 0x8784

pattern GL_OP_DOT4_EXT  :: (Eq a, Num a) => a

pattern GL_OP_DOT4_EXT = 0x8785

pattern GL_OP_EXP_BASE_2_EXT  :: (Eq a, Num a) => a

pattern GL_OP_EXP_BASE_2_EXT = 0x8791

pattern GL_OP_FLOOR_EXT  :: (Eq a, Num a) => a

pattern GL_OP_FLOOR_EXT = 0x878F

pattern GL_OP_FRAC_EXT  :: (Eq a, Num a) => a

pattern GL_OP_FRAC_EXT = 0x8789

pattern GL_OP_INDEX_EXT  :: (Eq a, Num a) => a

pattern GL_OP_INDEX_EXT = 0x8782

pattern GL_OP_LOG_BASE_2_EXT  :: (Eq a, Num a) => a

pattern GL_OP_LOG_BASE_2_EXT = 0x8792

pattern GL_OP_MADD_EXT  :: (Eq a, Num a) => a

pattern GL_OP_MADD_EXT = 0x8788

pattern GL_OP_MAX_EXT  :: (Eq a, Num a) => a

pattern GL_OP_MAX_EXT = 0x878A

pattern GL_OP_MIN_EXT  :: (Eq a, Num a) => a

pattern GL_OP_MIN_EXT = 0x878B

pattern GL_OP_MOV_EXT  :: (Eq a, Num a) => a

pattern GL_OP_MOV_EXT = 0x8799

pattern GL_OP_MULTIPLY_MATRIX_EXT  :: (Eq a, Num a) => a

pattern GL_OP_MULTIPLY_MATRIX_EXT = 0x8798

pattern GL_OP_MUL_EXT  :: (Eq a, Num a) => a

pattern GL_OP_MUL_EXT = 0x8786

pattern GL_OP_NEGATE_EXT  :: (Eq a, Num a) => a

pattern GL_OP_NEGATE_EXT = 0x8783

pattern GL_OP_POWER_EXT  :: (Eq a, Num a) => a

pattern GL_OP_POWER_EXT = 0x8793

pattern GL_OP_RECIP_EXT  :: (Eq a, Num a) => a

pattern GL_OP_RECIP_EXT = 0x8794

pattern GL_OP_RECIP_SQRT_EXT  :: (Eq a, Num a) => a

pattern GL_OP_RECIP_SQRT_EXT = 0x8795

pattern GL_OP_ROUND_EXT  :: (Eq a, Num a) => a

pattern GL_OP_ROUND_EXT = 0x8790

pattern GL_OP_SET_GE_EXT  :: (Eq a, Num a) => a

pattern GL_OP_SET_GE_EXT = 0x878C

pattern GL_OP_SET_LT_EXT  :: (Eq a, Num a) => a

pattern GL_OP_SET_LT_EXT = 0x878D

pattern GL_OP_SUB_EXT  :: (Eq a, Num a) => a

pattern GL_OP_SUB_EXT = 0x8796

pattern GL_OUTPUT_COLOR0_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_COLOR0_EXT = 0x879B

pattern GL_OUTPUT_COLOR1_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_COLOR1_EXT = 0x879C

pattern GL_OUTPUT_FOG_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_FOG_EXT = 0x87BD

pattern GL_OUTPUT_TEXTURE_COORD0_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD0_EXT = 0x879D

pattern GL_OUTPUT_TEXTURE_COORD10_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD10_EXT = 0x87A7

pattern GL_OUTPUT_TEXTURE_COORD11_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD11_EXT = 0x87A8

pattern GL_OUTPUT_TEXTURE_COORD12_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD12_EXT = 0x87A9

pattern GL_OUTPUT_TEXTURE_COORD13_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD13_EXT = 0x87AA

pattern GL_OUTPUT_TEXTURE_COORD14_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD14_EXT = 0x87AB

pattern GL_OUTPUT_TEXTURE_COORD15_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD15_EXT = 0x87AC

pattern GL_OUTPUT_TEXTURE_COORD16_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD16_EXT = 0x87AD

pattern GL_OUTPUT_TEXTURE_COORD17_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD17_EXT = 0x87AE

pattern GL_OUTPUT_TEXTURE_COORD18_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD18_EXT = 0x87AF

pattern GL_OUTPUT_TEXTURE_COORD19_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD19_EXT = 0x87B0

pattern GL_OUTPUT_TEXTURE_COORD1_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD1_EXT = 0x879E

pattern GL_OUTPUT_TEXTURE_COORD20_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD20_EXT = 0x87B1

pattern GL_OUTPUT_TEXTURE_COORD21_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD21_EXT = 0x87B2

pattern GL_OUTPUT_TEXTURE_COORD22_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD22_EXT = 0x87B3

pattern GL_OUTPUT_TEXTURE_COORD23_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD23_EXT = 0x87B4

pattern GL_OUTPUT_TEXTURE_COORD24_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD24_EXT = 0x87B5

pattern GL_OUTPUT_TEXTURE_COORD25_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD25_EXT = 0x87B6

pattern GL_OUTPUT_TEXTURE_COORD26_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD26_EXT = 0x87B7

pattern GL_OUTPUT_TEXTURE_COORD27_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD27_EXT = 0x87B8

pattern GL_OUTPUT_TEXTURE_COORD28_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD28_EXT = 0x87B9

pattern GL_OUTPUT_TEXTURE_COORD29_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD29_EXT = 0x87BA

pattern GL_OUTPUT_TEXTURE_COORD2_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD2_EXT = 0x879F

pattern GL_OUTPUT_TEXTURE_COORD30_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD30_EXT = 0x87BB

pattern GL_OUTPUT_TEXTURE_COORD31_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD31_EXT = 0x87BC

pattern GL_OUTPUT_TEXTURE_COORD3_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD3_EXT = 0x87A0

pattern GL_OUTPUT_TEXTURE_COORD4_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD4_EXT = 0x87A1

pattern GL_OUTPUT_TEXTURE_COORD5_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD5_EXT = 0x87A2

pattern GL_OUTPUT_TEXTURE_COORD6_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD6_EXT = 0x87A3

pattern GL_OUTPUT_TEXTURE_COORD7_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD7_EXT = 0x87A4

pattern GL_OUTPUT_TEXTURE_COORD8_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD8_EXT = 0x87A5

pattern GL_OUTPUT_TEXTURE_COORD9_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_TEXTURE_COORD9_EXT = 0x87A6

pattern GL_OUTPUT_VERTEX_EXT  :: (Eq a, Num a) => a

pattern GL_OUTPUT_VERTEX_EXT = 0x879A

pattern GL_SCALAR_EXT  :: (Eq a, Num a) => a

pattern GL_SCALAR_EXT = 0x87BE

pattern GL_VARIANT_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_ARRAY_EXT = 0x87E8

pattern GL_VARIANT_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_ARRAY_POINTER_EXT = 0x87E9

pattern GL_VARIANT_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_ARRAY_STRIDE_EXT = 0x87E6

pattern GL_VARIANT_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_ARRAY_TYPE_EXT = 0x87E7

pattern GL_VARIANT_DATATYPE_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_DATATYPE_EXT = 0x87E5

pattern GL_VARIANT_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_EXT = 0x87C1

pattern GL_VARIANT_VALUE_EXT  :: (Eq a, Num a) => a

pattern GL_VARIANT_VALUE_EXT = 0x87E4

pattern GL_VECTOR_EXT  :: (Eq a, Num a) => a

pattern GL_VECTOR_EXT = 0x87BF

pattern GL_VERTEX_SHADER_BINDING_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_BINDING_EXT = 0x8781

pattern GL_VERTEX_SHADER_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_EXT = 0x8780

pattern GL_VERTEX_SHADER_INSTRUCTIONS_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_INSTRUCTIONS_EXT = 0x87CF

pattern GL_VERTEX_SHADER_INVARIANTS_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_INVARIANTS_EXT = 0x87D1

pattern GL_VERTEX_SHADER_LOCALS_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_LOCALS_EXT = 0x87D3

pattern GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT = 0x87D2

pattern GL_VERTEX_SHADER_OPTIMIZED_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_OPTIMIZED_EXT = 0x87D4

pattern GL_VERTEX_SHADER_VARIANTS_EXT  :: (Eq a, Num a) => a

pattern GL_VERTEX_SHADER_VARIANTS_EXT = 0x87D0

pattern GL_W_EXT  :: (Eq a, Num a) => a

pattern GL_W_EXT = 0x87D8

pattern GL_X_EXT  :: (Eq a, Num a) => a

pattern GL_X_EXT = 0x87D5

pattern GL_Y_EXT  :: (Eq a, Num a) => a

pattern GL_Y_EXT = 0x87D6

pattern GL_ZERO_EXT  :: (Eq a, Num a) => a

pattern GL_ZERO_EXT = 0x87DD

pattern GL_Z_EXT  :: (Eq a, Num a) => a

pattern GL_Z_EXT = 0x87D7