-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ATI.FragmentShader (
  -- * Extension Support
    gl_ATI_fragment_shader

  -- * GL_ATI_fragment_shader
  , glAlphaFragmentOp1ATI
  , glAlphaFragmentOp2ATI
  , glAlphaFragmentOp3ATI
  , glBeginFragmentShaderATI
  , glBindFragmentShaderATI
  , glColorFragmentOp1ATI
  , glColorFragmentOp2ATI
  , glColorFragmentOp3ATI
  , glDeleteFragmentShaderATI
  , glEndFragmentShaderATI
  , glGenFragmentShadersATI
  , glPassTexCoordATI
  , glSampleMapATI
  , glSetFragmentShaderConstantATI
  , pattern GL_2X_BIT_ATI
  , pattern GL_4X_BIT_ATI
  , pattern GL_8X_BIT_ATI
  , pattern GL_ADD_ATI
  , pattern GL_BIAS_BIT_ATI
  , pattern GL_BLUE_BIT_ATI
  , pattern GL_CND0_ATI
  , pattern GL_CND_ATI
  , pattern GL_COLOR_ALPHA_PAIRING_ATI
  , pattern GL_COMP_BIT_ATI
  , pattern GL_CON_0_ATI
  , pattern GL_CON_10_ATI
  , pattern GL_CON_11_ATI
  , pattern GL_CON_12_ATI
  , pattern GL_CON_13_ATI
  , pattern GL_CON_14_ATI
  , pattern GL_CON_15_ATI
  , pattern GL_CON_16_ATI
  , pattern GL_CON_17_ATI
  , pattern GL_CON_18_ATI
  , pattern GL_CON_19_ATI
  , pattern GL_CON_1_ATI
  , pattern GL_CON_20_ATI
  , pattern GL_CON_21_ATI
  , pattern GL_CON_22_ATI
  , pattern GL_CON_23_ATI
  , pattern GL_CON_24_ATI
  , pattern GL_CON_25_ATI
  , pattern GL_CON_26_ATI
  , pattern GL_CON_27_ATI
  , pattern GL_CON_28_ATI
  , pattern GL_CON_29_ATI
  , pattern GL_CON_2_ATI
  , pattern GL_CON_30_ATI
  , pattern GL_CON_31_ATI
  , pattern GL_CON_3_ATI
  , pattern GL_CON_4_ATI
  , pattern GL_CON_5_ATI
  , pattern GL_CON_6_ATI
  , pattern GL_CON_7_ATI
  , pattern GL_CON_8_ATI
  , pattern GL_CON_9_ATI
  , pattern GL_DOT2_ADD_ATI
  , pattern GL_DOT3_ATI
  , pattern GL_DOT4_ATI
  , pattern GL_EIGHTH_BIT_ATI
  , pattern GL_FRAGMENT_SHADER_ATI
  , pattern GL_GREEN_BIT_ATI
  , pattern GL_HALF_BIT_ATI
  , pattern GL_LERP_ATI
  , pattern GL_MAD_ATI
  , pattern GL_MOV_ATI
  , pattern GL_MUL_ATI
  , pattern GL_NEGATE_BIT_ATI
  , pattern GL_NUM_FRAGMENT_CONSTANTS_ATI
  , pattern GL_NUM_FRAGMENT_REGISTERS_ATI
  , pattern GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI
  , pattern GL_NUM_INSTRUCTIONS_PER_PASS_ATI
  , pattern GL_NUM_INSTRUCTIONS_TOTAL_ATI
  , pattern GL_NUM_LOOPBACK_COMPONENTS_ATI
  , pattern GL_NUM_PASSES_ATI
  , pattern GL_QUARTER_BIT_ATI
  , pattern GL_RED_BIT_ATI
  , pattern GL_REG_0_ATI
  , pattern GL_REG_10_ATI
  , pattern GL_REG_11_ATI
  , pattern GL_REG_12_ATI
  , pattern GL_REG_13_ATI
  , pattern GL_REG_14_ATI
  , pattern GL_REG_15_ATI
  , pattern GL_REG_16_ATI
  , pattern GL_REG_17_ATI
  , pattern GL_REG_18_ATI
  , pattern GL_REG_19_ATI
  , pattern GL_REG_1_ATI
  , pattern GL_REG_20_ATI
  , pattern GL_REG_21_ATI
  , pattern GL_REG_22_ATI
  , pattern GL_REG_23_ATI
  , pattern GL_REG_24_ATI
  , pattern GL_REG_25_ATI
  , pattern GL_REG_26_ATI
  , pattern GL_REG_27_ATI
  , pattern GL_REG_28_ATI
  , pattern GL_REG_29_ATI
  , pattern GL_REG_2_ATI
  , pattern GL_REG_30_ATI
  , pattern GL_REG_31_ATI
  , pattern GL_REG_3_ATI
  , pattern GL_REG_4_ATI
  , pattern GL_REG_5_ATI
  , pattern GL_REG_6_ATI
  , pattern GL_REG_7_ATI
  , pattern GL_REG_8_ATI
  , pattern GL_REG_9_ATI
  , pattern GL_SATURATE_BIT_ATI
  , pattern GL_SECONDARY_INTERPOLATOR_ATI
  , pattern GL_SUB_ATI
  , pattern GL_SWIZZLE_STQ_ATI
  , pattern GL_SWIZZLE_STQ_DQ_ATI
  , pattern GL_SWIZZLE_STRQ_ATI
  , pattern GL_SWIZZLE_STRQ_DQ_ATI
  , pattern GL_SWIZZLE_STR_ATI
  , pattern GL_SWIZZLE_STR_DR_ATI
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ATI/fragment_shader.txt GL_ATI_fragment_shader> extension is available.

gl_ATI_fragment_shader :: Bool
gl_ATI_fragment_shader = member "GL_ATI_fragment_shader" extensions
{-# NOINLINE gl_ATI_fragment_shader #-}

-- | Usage: @'glAlphaFragmentOp1ATI' op dst dstMod arg1 arg1Rep arg1Mod@
--
-- The parameter @op@ is a @FragmentOpATI@.


glAlphaFragmentOp1ATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glAlphaFragmentOp1ATI = ffienumuintuintuintuintuintIOV glAlphaFragmentOp1ATIFunPtr

glAlphaFragmentOp1ATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glAlphaFragmentOp1ATIFunPtr = unsafePerformIO (getProcAddress "glAlphaFragmentOp1ATI")

{-# NOINLINE glAlphaFragmentOp1ATIFunPtr #-}

-- | Usage: @'glAlphaFragmentOp2ATI' op dst dstMod arg1 arg1Rep arg1Mod arg2 arg2Rep arg2Mod@
--
-- The parameter @op@ is a @FragmentOpATI@.


glAlphaFragmentOp2ATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glAlphaFragmentOp2ATI = ffienumuintuintuintuintuintuintuintuintIOV glAlphaFragmentOp2ATIFunPtr

glAlphaFragmentOp2ATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glAlphaFragmentOp2ATIFunPtr = unsafePerformIO (getProcAddress "glAlphaFragmentOp2ATI")

{-# NOINLINE glAlphaFragmentOp2ATIFunPtr #-}

-- | Usage: @'glAlphaFragmentOp3ATI' op dst dstMod arg1 arg1Rep arg1Mod arg2 arg2Rep arg2Mod arg3 arg3Rep arg3Mod@
--
-- The parameter @op@ is a @FragmentOpATI@.


glAlphaFragmentOp3ATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glAlphaFragmentOp3ATI = ffienumuintuintuintuintuintuintuintuintuintuintuintIOV glAlphaFragmentOp3ATIFunPtr

glAlphaFragmentOp3ATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glAlphaFragmentOp3ATIFunPtr = unsafePerformIO (getProcAddress "glAlphaFragmentOp3ATI")

{-# NOINLINE glAlphaFragmentOp3ATIFunPtr #-}

-- | Usage: @'glBeginFragmentShaderATI'@


glBeginFragmentShaderATI :: MonadIO m => m ()
glBeginFragmentShaderATI = ffiIOV glBeginFragmentShaderATIFunPtr

glBeginFragmentShaderATIFunPtr :: FunPtr (IO ())
glBeginFragmentShaderATIFunPtr = unsafePerformIO (getProcAddress "glBeginFragmentShaderATI")

{-# NOINLINE glBeginFragmentShaderATIFunPtr #-}

-- | Usage: @'glBindFragmentShaderATI' id@


glBindFragmentShaderATI :: MonadIO m => GLuint -> m ()
glBindFragmentShaderATI = ffiuintIOV glBindFragmentShaderATIFunPtr

glBindFragmentShaderATIFunPtr :: FunPtr (GLuint -> IO ())
glBindFragmentShaderATIFunPtr = unsafePerformIO (getProcAddress "glBindFragmentShaderATI")

{-# NOINLINE glBindFragmentShaderATIFunPtr #-}

-- | Usage: @'glColorFragmentOp1ATI' op dst dstMask dstMod arg1 arg1Rep arg1Mod@
--
-- The parameter @op@ is a @FragmentOpATI@.


glColorFragmentOp1ATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glColorFragmentOp1ATI = ffienumuintuintuintuintuintuintIOV glColorFragmentOp1ATIFunPtr

glColorFragmentOp1ATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glColorFragmentOp1ATIFunPtr = unsafePerformIO (getProcAddress "glColorFragmentOp1ATI")

{-# NOINLINE glColorFragmentOp1ATIFunPtr #-}

-- | Usage: @'glColorFragmentOp2ATI' op dst dstMask dstMod arg1 arg1Rep arg1Mod arg2 arg2Rep arg2Mod@
--
-- The parameter @op@ is a @FragmentOpATI@.


glColorFragmentOp2ATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glColorFragmentOp2ATI = ffienumuintuintuintuintuintuintuintuintuintIOV glColorFragmentOp2ATIFunPtr

glColorFragmentOp2ATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glColorFragmentOp2ATIFunPtr = unsafePerformIO (getProcAddress "glColorFragmentOp2ATI")

{-# NOINLINE glColorFragmentOp2ATIFunPtr #-}

-- | Usage: @'glColorFragmentOp3ATI' op dst dstMask dstMod arg1 arg1Rep arg1Mod arg2 arg2Rep arg2Mod arg3 arg3Rep arg3Mod@
--
-- The parameter @op@ is a @FragmentOpATI@.


glColorFragmentOp3ATI :: MonadIO m => GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> m ()
glColorFragmentOp3ATI = ffienumuintuintuintuintuintuintuintuintuintuintuintuintIOV glColorFragmentOp3ATIFunPtr

glColorFragmentOp3ATIFunPtr :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
glColorFragmentOp3ATIFunPtr = unsafePerformIO (getProcAddress "glColorFragmentOp3ATI")

{-# NOINLINE glColorFragmentOp3ATIFunPtr #-}

-- | Usage: @'glDeleteFragmentShaderATI' id@


glDeleteFragmentShaderATI :: MonadIO m => GLuint -> m ()
glDeleteFragmentShaderATI = ffiuintIOV glDeleteFragmentShaderATIFunPtr

glDeleteFragmentShaderATIFunPtr :: FunPtr (GLuint -> IO ())
glDeleteFragmentShaderATIFunPtr = unsafePerformIO (getProcAddress "glDeleteFragmentShaderATI")

{-# NOINLINE glDeleteFragmentShaderATIFunPtr #-}

-- | Usage: @'glEndFragmentShaderATI'@


glEndFragmentShaderATI :: MonadIO m => m ()
glEndFragmentShaderATI = ffiIOV glEndFragmentShaderATIFunPtr

glEndFragmentShaderATIFunPtr :: FunPtr (IO ())
glEndFragmentShaderATIFunPtr = unsafePerformIO (getProcAddress "glEndFragmentShaderATI")

{-# NOINLINE glEndFragmentShaderATIFunPtr #-}

-- | Usage: @'glGenFragmentShadersATI' range@


glGenFragmentShadersATI :: MonadIO m => GLuint -> m GLuint
glGenFragmentShadersATI = ffiuintIOuint glGenFragmentShadersATIFunPtr

glGenFragmentShadersATIFunPtr :: FunPtr (GLuint -> IO GLuint)
glGenFragmentShadersATIFunPtr = unsafePerformIO (getProcAddress "glGenFragmentShadersATI")

{-# NOINLINE glGenFragmentShadersATIFunPtr #-}

-- | Usage: @'glPassTexCoordATI' dst coord swizzle@
--
-- The parameter @swizzle@ is a @SwizzleOpATI@.


glPassTexCoordATI :: MonadIO m => GLuint -> GLuint -> GLenum -> m ()
glPassTexCoordATI = ffiuintuintenumIOV glPassTexCoordATIFunPtr

glPassTexCoordATIFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> IO ())
glPassTexCoordATIFunPtr = unsafePerformIO (getProcAddress "glPassTexCoordATI")

{-# NOINLINE glPassTexCoordATIFunPtr #-}

-- | Usage: @'glSampleMapATI' dst interp swizzle@
--
-- The parameter @swizzle@ is a @SwizzleOpATI@.


glSampleMapATI :: MonadIO m => GLuint -> GLuint -> GLenum -> m ()
glSampleMapATI = ffiuintuintenumIOV glSampleMapATIFunPtr

glSampleMapATIFunPtr :: FunPtr (GLuint -> GLuint -> GLenum -> IO ())
glSampleMapATIFunPtr = unsafePerformIO (getProcAddress "glSampleMapATI")

{-# NOINLINE glSampleMapATIFunPtr #-}

-- | Usage: @'glSetFragmentShaderConstantATI' dst value@
--
-- The length of @value@ should be @4@.


glSetFragmentShaderConstantATI :: MonadIO m => GLuint -> Ptr GLfloat -> m ()
glSetFragmentShaderConstantATI = ffiuintPtrfloatIOV glSetFragmentShaderConstantATIFunPtr

glSetFragmentShaderConstantATIFunPtr :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
glSetFragmentShaderConstantATIFunPtr = unsafePerformIO (getProcAddress "glSetFragmentShaderConstantATI")

{-# NOINLINE glSetFragmentShaderConstantATIFunPtr #-}

pattern GL_2X_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_2X_BIT_ATI = 0x00000001

pattern GL_4X_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_4X_BIT_ATI = 0x00000002

pattern GL_8X_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_8X_BIT_ATI = 0x00000004

pattern GL_ADD_ATI  :: (Eq a, Num a) => a

pattern GL_ADD_ATI = 0x8963

pattern GL_BIAS_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_BIAS_BIT_ATI = 0x00000008

pattern GL_BLUE_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_BLUE_BIT_ATI = 0x00000004

pattern GL_CND0_ATI  :: (Eq a, Num a) => a

pattern GL_CND0_ATI = 0x896B

pattern GL_CND_ATI  :: (Eq a, Num a) => a

pattern GL_CND_ATI = 0x896A

pattern GL_COLOR_ALPHA_PAIRING_ATI  :: (Eq a, Num a) => a

pattern GL_COLOR_ALPHA_PAIRING_ATI = 0x8975

pattern GL_COMP_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_COMP_BIT_ATI = 0x00000002

pattern GL_CON_0_ATI  :: (Eq a, Num a) => a

pattern GL_CON_0_ATI = 0x8941

pattern GL_CON_10_ATI  :: (Eq a, Num a) => a

pattern GL_CON_10_ATI = 0x894B

pattern GL_CON_11_ATI  :: (Eq a, Num a) => a

pattern GL_CON_11_ATI = 0x894C

pattern GL_CON_12_ATI  :: (Eq a, Num a) => a

pattern GL_CON_12_ATI = 0x894D

pattern GL_CON_13_ATI  :: (Eq a, Num a) => a

pattern GL_CON_13_ATI = 0x894E

pattern GL_CON_14_ATI  :: (Eq a, Num a) => a

pattern GL_CON_14_ATI = 0x894F

pattern GL_CON_15_ATI  :: (Eq a, Num a) => a

pattern GL_CON_15_ATI = 0x8950

pattern GL_CON_16_ATI  :: (Eq a, Num a) => a

pattern GL_CON_16_ATI = 0x8951

pattern GL_CON_17_ATI  :: (Eq a, Num a) => a

pattern GL_CON_17_ATI = 0x8952

pattern GL_CON_18_ATI  :: (Eq a, Num a) => a

pattern GL_CON_18_ATI = 0x8953

pattern GL_CON_19_ATI  :: (Eq a, Num a) => a

pattern GL_CON_19_ATI = 0x8954

pattern GL_CON_1_ATI  :: (Eq a, Num a) => a

pattern GL_CON_1_ATI = 0x8942

pattern GL_CON_20_ATI  :: (Eq a, Num a) => a

pattern GL_CON_20_ATI = 0x8955

pattern GL_CON_21_ATI  :: (Eq a, Num a) => a

pattern GL_CON_21_ATI = 0x8956

pattern GL_CON_22_ATI  :: (Eq a, Num a) => a

pattern GL_CON_22_ATI = 0x8957

pattern GL_CON_23_ATI  :: (Eq a, Num a) => a

pattern GL_CON_23_ATI = 0x8958

pattern GL_CON_24_ATI  :: (Eq a, Num a) => a

pattern GL_CON_24_ATI = 0x8959

pattern GL_CON_25_ATI  :: (Eq a, Num a) => a

pattern GL_CON_25_ATI = 0x895A

pattern GL_CON_26_ATI  :: (Eq a, Num a) => a

pattern GL_CON_26_ATI = 0x895B

pattern GL_CON_27_ATI  :: (Eq a, Num a) => a

pattern GL_CON_27_ATI = 0x895C

pattern GL_CON_28_ATI  :: (Eq a, Num a) => a

pattern GL_CON_28_ATI = 0x895D

pattern GL_CON_29_ATI  :: (Eq a, Num a) => a

pattern GL_CON_29_ATI = 0x895E

pattern GL_CON_2_ATI  :: (Eq a, Num a) => a

pattern GL_CON_2_ATI = 0x8943

pattern GL_CON_30_ATI  :: (Eq a, Num a) => a

pattern GL_CON_30_ATI = 0x895F

pattern GL_CON_31_ATI  :: (Eq a, Num a) => a

pattern GL_CON_31_ATI = 0x8960

pattern GL_CON_3_ATI  :: (Eq a, Num a) => a

pattern GL_CON_3_ATI = 0x8944

pattern GL_CON_4_ATI  :: (Eq a, Num a) => a

pattern GL_CON_4_ATI = 0x8945

pattern GL_CON_5_ATI  :: (Eq a, Num a) => a

pattern GL_CON_5_ATI = 0x8946

pattern GL_CON_6_ATI  :: (Eq a, Num a) => a

pattern GL_CON_6_ATI = 0x8947

pattern GL_CON_7_ATI  :: (Eq a, Num a) => a

pattern GL_CON_7_ATI = 0x8948

pattern GL_CON_8_ATI  :: (Eq a, Num a) => a

pattern GL_CON_8_ATI = 0x8949

pattern GL_CON_9_ATI  :: (Eq a, Num a) => a

pattern GL_CON_9_ATI = 0x894A

pattern GL_DOT2_ADD_ATI  :: (Eq a, Num a) => a

pattern GL_DOT2_ADD_ATI = 0x896C

pattern GL_DOT3_ATI  :: (Eq a, Num a) => a

pattern GL_DOT3_ATI = 0x8966

pattern GL_DOT4_ATI  :: (Eq a, Num a) => a

pattern GL_DOT4_ATI = 0x8967

pattern GL_EIGHTH_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_EIGHTH_BIT_ATI = 0x00000020

pattern GL_FRAGMENT_SHADER_ATI  :: (Eq a, Num a) => a

pattern GL_FRAGMENT_SHADER_ATI = 0x8920

pattern GL_GREEN_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_GREEN_BIT_ATI = 0x00000002

pattern GL_HALF_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_HALF_BIT_ATI = 0x00000008

pattern GL_LERP_ATI  :: (Eq a, Num a) => a

pattern GL_LERP_ATI = 0x8969

pattern GL_MAD_ATI  :: (Eq a, Num a) => a

pattern GL_MAD_ATI = 0x8968

pattern GL_MOV_ATI  :: (Eq a, Num a) => a

pattern GL_MOV_ATI = 0x8961

pattern GL_MUL_ATI  :: (Eq a, Num a) => a

pattern GL_MUL_ATI = 0x8964

pattern GL_NEGATE_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_NEGATE_BIT_ATI = 0x00000004

pattern GL_NUM_FRAGMENT_CONSTANTS_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_FRAGMENT_CONSTANTS_ATI = 0x896F

pattern GL_NUM_FRAGMENT_REGISTERS_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_FRAGMENT_REGISTERS_ATI = 0x896E

pattern GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI = 0x8973

pattern GL_NUM_INSTRUCTIONS_PER_PASS_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_INSTRUCTIONS_PER_PASS_ATI = 0x8971

pattern GL_NUM_INSTRUCTIONS_TOTAL_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_INSTRUCTIONS_TOTAL_ATI = 0x8972

pattern GL_NUM_LOOPBACK_COMPONENTS_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_LOOPBACK_COMPONENTS_ATI = 0x8974

pattern GL_NUM_PASSES_ATI  :: (Eq a, Num a) => a

pattern GL_NUM_PASSES_ATI = 0x8970

pattern GL_QUARTER_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_QUARTER_BIT_ATI = 0x00000010

pattern GL_RED_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_RED_BIT_ATI = 0x00000001

pattern GL_REG_0_ATI  :: (Eq a, Num a) => a

pattern GL_REG_0_ATI = 0x8921

pattern GL_REG_10_ATI  :: (Eq a, Num a) => a

pattern GL_REG_10_ATI = 0x892B

pattern GL_REG_11_ATI  :: (Eq a, Num a) => a

pattern GL_REG_11_ATI = 0x892C

pattern GL_REG_12_ATI  :: (Eq a, Num a) => a

pattern GL_REG_12_ATI = 0x892D

pattern GL_REG_13_ATI  :: (Eq a, Num a) => a

pattern GL_REG_13_ATI = 0x892E

pattern GL_REG_14_ATI  :: (Eq a, Num a) => a

pattern GL_REG_14_ATI = 0x892F

pattern GL_REG_15_ATI  :: (Eq a, Num a) => a

pattern GL_REG_15_ATI = 0x8930

pattern GL_REG_16_ATI  :: (Eq a, Num a) => a

pattern GL_REG_16_ATI = 0x8931

pattern GL_REG_17_ATI  :: (Eq a, Num a) => a

pattern GL_REG_17_ATI = 0x8932

pattern GL_REG_18_ATI  :: (Eq a, Num a) => a

pattern GL_REG_18_ATI = 0x8933

pattern GL_REG_19_ATI  :: (Eq a, Num a) => a

pattern GL_REG_19_ATI = 0x8934

pattern GL_REG_1_ATI  :: (Eq a, Num a) => a

pattern GL_REG_1_ATI = 0x8922

pattern GL_REG_20_ATI  :: (Eq a, Num a) => a

pattern GL_REG_20_ATI = 0x8935

pattern GL_REG_21_ATI  :: (Eq a, Num a) => a

pattern GL_REG_21_ATI = 0x8936

pattern GL_REG_22_ATI  :: (Eq a, Num a) => a

pattern GL_REG_22_ATI = 0x8937

pattern GL_REG_23_ATI  :: (Eq a, Num a) => a

pattern GL_REG_23_ATI = 0x8938

pattern GL_REG_24_ATI  :: (Eq a, Num a) => a

pattern GL_REG_24_ATI = 0x8939

pattern GL_REG_25_ATI  :: (Eq a, Num a) => a

pattern GL_REG_25_ATI = 0x893A

pattern GL_REG_26_ATI  :: (Eq a, Num a) => a

pattern GL_REG_26_ATI = 0x893B

pattern GL_REG_27_ATI  :: (Eq a, Num a) => a

pattern GL_REG_27_ATI = 0x893C

pattern GL_REG_28_ATI  :: (Eq a, Num a) => a

pattern GL_REG_28_ATI = 0x893D

pattern GL_REG_29_ATI  :: (Eq a, Num a) => a

pattern GL_REG_29_ATI = 0x893E

pattern GL_REG_2_ATI  :: (Eq a, Num a) => a

pattern GL_REG_2_ATI = 0x8923

pattern GL_REG_30_ATI  :: (Eq a, Num a) => a

pattern GL_REG_30_ATI = 0x893F

pattern GL_REG_31_ATI  :: (Eq a, Num a) => a

pattern GL_REG_31_ATI = 0x8940

pattern GL_REG_3_ATI  :: (Eq a, Num a) => a

pattern GL_REG_3_ATI = 0x8924

pattern GL_REG_4_ATI  :: (Eq a, Num a) => a

pattern GL_REG_4_ATI = 0x8925

pattern GL_REG_5_ATI  :: (Eq a, Num a) => a

pattern GL_REG_5_ATI = 0x8926

pattern GL_REG_6_ATI  :: (Eq a, Num a) => a

pattern GL_REG_6_ATI = 0x8927

pattern GL_REG_7_ATI  :: (Eq a, Num a) => a

pattern GL_REG_7_ATI = 0x8928

pattern GL_REG_8_ATI  :: (Eq a, Num a) => a

pattern GL_REG_8_ATI = 0x8929

pattern GL_REG_9_ATI  :: (Eq a, Num a) => a

pattern GL_REG_9_ATI = 0x892A

pattern GL_SATURATE_BIT_ATI  :: (Eq a, Num a) => a

pattern GL_SATURATE_BIT_ATI = 0x00000040

pattern GL_SECONDARY_INTERPOLATOR_ATI  :: (Eq a, Num a) => a

pattern GL_SECONDARY_INTERPOLATOR_ATI = 0x896D

pattern GL_SUB_ATI  :: (Eq a, Num a) => a

pattern GL_SUB_ATI = 0x8965

pattern GL_SWIZZLE_STQ_ATI  :: (Eq a, Num a) => a

pattern GL_SWIZZLE_STQ_ATI = 0x8977

pattern GL_SWIZZLE_STQ_DQ_ATI  :: (Eq a, Num a) => a

pattern GL_SWIZZLE_STQ_DQ_ATI = 0x8979

pattern GL_SWIZZLE_STRQ_ATI  :: (Eq a, Num a) => a

pattern GL_SWIZZLE_STRQ_ATI = 0x897A

pattern GL_SWIZZLE_STRQ_DQ_ATI  :: (Eq a, Num a) => a

pattern GL_SWIZZLE_STRQ_DQ_ATI = 0x897B

pattern GL_SWIZZLE_STR_ATI  :: (Eq a, Num a) => a

pattern GL_SWIZZLE_STR_ATI = 0x8976

pattern GL_SWIZZLE_STR_DR_ATI  :: (Eq a, Num a) => a

pattern GL_SWIZZLE_STR_DR_ATI = 0x8978