-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.RegisterCombiners (
  -- * Extension Support
    gl_NV_register_combiners

  -- * GL_NV_register_combiners
  , glCombinerInputNV
  , glCombinerOutputNV
  , glCombinerParameterfNV
  , glCombinerParameterfvNV
  , glCombinerParameteriNV
  , glCombinerParameterivNV
  , glFinalCombinerInputNV
  , glGetCombinerInputParameterfvNV
  , glGetCombinerInputParameterivNV
  , glGetCombinerOutputParameterfvNV
  , glGetCombinerOutputParameterivNV
  , glGetFinalCombinerInputParameterfvNV
  , glGetFinalCombinerInputParameterivNV
  , pattern GL_BIAS_BY_NEGATIVE_ONE_HALF_NV
  , pattern GL_COLOR_SUM_CLAMP_NV
  , pattern GL_COMBINER0_NV
  , pattern GL_COMBINER1_NV
  , pattern GL_COMBINER2_NV
  , pattern GL_COMBINER3_NV
  , pattern GL_COMBINER4_NV
  , pattern GL_COMBINER5_NV
  , pattern GL_COMBINER6_NV
  , pattern GL_COMBINER7_NV
  , pattern GL_COMBINER_AB_DOT_PRODUCT_NV
  , pattern GL_COMBINER_AB_OUTPUT_NV
  , pattern GL_COMBINER_BIAS_NV
  , pattern GL_COMBINER_CD_DOT_PRODUCT_NV
  , pattern GL_COMBINER_CD_OUTPUT_NV
  , pattern GL_COMBINER_COMPONENT_USAGE_NV
  , pattern GL_COMBINER_INPUT_NV
  , pattern GL_COMBINER_MAPPING_NV
  , pattern GL_COMBINER_MUX_SUM_NV
  , pattern GL_COMBINER_SCALE_NV
  , pattern GL_COMBINER_SUM_OUTPUT_NV
  , pattern GL_CONSTANT_COLOR0_NV
  , pattern GL_CONSTANT_COLOR1_NV
  , pattern GL_DISCARD_NV
  , pattern GL_EXPAND_NEGATE_NV
  , pattern GL_EXPAND_NORMAL_NV
  , pattern GL_E_TIMES_F_NV
  , pattern GL_FOG
  , pattern GL_HALF_BIAS_NEGATE_NV
  , pattern GL_HALF_BIAS_NORMAL_NV
  , pattern GL_MAX_GENERAL_COMBINERS_NV
  , pattern GL_NONE
  , pattern GL_NUM_GENERAL_COMBINERS_NV
  , pattern GL_PRIMARY_COLOR_NV
  , pattern GL_REGISTER_COMBINERS_NV
  , pattern GL_SCALE_BY_FOUR_NV
  , pattern GL_SCALE_BY_ONE_HALF_NV
  , pattern GL_SCALE_BY_TWO_NV
  , pattern GL_SECONDARY_COLOR_NV
  , pattern GL_SIGNED_IDENTITY_NV
  , pattern GL_SIGNED_NEGATE_NV
  , pattern GL_SPARE0_NV
  , pattern GL_SPARE0_PLUS_SECONDARY_COLOR_NV
  , pattern GL_SPARE1_NV
  , pattern GL_TEXTURE0_ARB
  , pattern GL_TEXTURE1_ARB
  , pattern GL_UNSIGNED_IDENTITY_NV
  , pattern GL_UNSIGNED_INVERT_NV
  , pattern GL_VARIABLE_A_NV
  , pattern GL_VARIABLE_B_NV
  , pattern GL_VARIABLE_C_NV
  , pattern GL_VARIABLE_D_NV
  , pattern GL_VARIABLE_E_NV
  , pattern GL_VARIABLE_F_NV
  , pattern GL_VARIABLE_G_NV
  , pattern GL_ZERO
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/register_combiners.txt GL_NV_register_combiners> extension is available.

gl_NV_register_combiners :: Bool
gl_NV_register_combiners = member "GL_NV_register_combiners" extensions
{-# NOINLINE gl_NV_register_combiners #-}

-- | Usage: @'glCombinerInputNV' stage portion variable input mapping componentUsage@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @portion@ is a @CombinerPortionNV@.
--
-- The parameter @variable@ is a @CombinerVariableNV@.
--
-- The parameter @input@ is a @CombinerRegisterNV@.
--
-- The parameter @mapping@ is a @CombinerMappingNV@.
--
-- The parameter @componentUsage@ is a @CombinerComponentUsageNV@.


glCombinerInputNV :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> m ()
glCombinerInputNV = ffienumenumenumenumenumenumIOV glCombinerInputNVFunPtr

glCombinerInputNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glCombinerInputNVFunPtr = unsafePerformIO (getProcAddress "glCombinerInputNV")

{-# NOINLINE glCombinerInputNVFunPtr #-}

-- | Usage: @'glCombinerOutputNV' stage portion abOutput cdOutput sumOutput scale bias abDotProduct cdDotProduct muxSum@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @portion@ is a @CombinerPortionNV@.
--
-- The parameter @abOutput@ is a @CombinerRegisterNV@.
--
-- The parameter @cdOutput@ is a @CombinerRegisterNV@.
--
-- The parameter @sumOutput@ is a @CombinerRegisterNV@.
--
-- The parameter @scale@ is a @CombinerScaleNV@.
--
-- The parameter @bias@ is a @CombinerBiasNV@.
--
-- The parameter @abDotProduct@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @cdDotProduct@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- The parameter @muxSum@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glCombinerOutputNV :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> m ()
glCombinerOutputNV = ffienumenumenumenumenumenumenumbooleanbooleanbooleanIOV glCombinerOutputNVFunPtr

glCombinerOutputNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> IO ())
glCombinerOutputNVFunPtr = unsafePerformIO (getProcAddress "glCombinerOutputNV")

{-# NOINLINE glCombinerOutputNVFunPtr #-}

-- | Usage: @'glCombinerParameterfNV' pname param@
--
-- The parameter @pname@ is a @CombinerParameterNV@.


glCombinerParameterfNV :: MonadIO m => GLenum -> GLfloat -> m ()
glCombinerParameterfNV = ffienumfloatIOV glCombinerParameterfNVFunPtr

glCombinerParameterfNVFunPtr :: FunPtr (GLenum -> GLfloat -> IO ())
glCombinerParameterfNVFunPtr = unsafePerformIO (getProcAddress "glCombinerParameterfNV")

{-# NOINLINE glCombinerParameterfNVFunPtr #-}

-- | Usage: @'glCombinerParameterfvNV' pname params@
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glCombinerParameterfvNV :: MonadIO m => GLenum -> Ptr GLfloat -> m ()
glCombinerParameterfvNV = ffienumPtrfloatIOV glCombinerParameterfvNVFunPtr

glCombinerParameterfvNVFunPtr :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
glCombinerParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glCombinerParameterfvNV")

{-# NOINLINE glCombinerParameterfvNVFunPtr #-}

-- | Usage: @'glCombinerParameteriNV' pname param@
--
-- The parameter @pname@ is a @CombinerParameterNV@.


glCombinerParameteriNV :: MonadIO m => GLenum -> GLint -> m ()
glCombinerParameteriNV = ffienumintIOV glCombinerParameteriNVFunPtr

glCombinerParameteriNVFunPtr :: FunPtr (GLenum -> GLint -> IO ())
glCombinerParameteriNVFunPtr = unsafePerformIO (getProcAddress "glCombinerParameteriNV")

{-# NOINLINE glCombinerParameteriNVFunPtr #-}

-- | Usage: @'glCombinerParameterivNV' pname params@
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glCombinerParameterivNV :: MonadIO m => GLenum -> Ptr GLint -> m ()
glCombinerParameterivNV = ffienumPtrintIOV glCombinerParameterivNVFunPtr

glCombinerParameterivNVFunPtr :: FunPtr (GLenum -> Ptr GLint -> IO ())
glCombinerParameterivNVFunPtr = unsafePerformIO (getProcAddress "glCombinerParameterivNV")

{-# NOINLINE glCombinerParameterivNVFunPtr #-}

-- | Usage: @'glFinalCombinerInputNV' variable input mapping componentUsage@
--
-- The parameter @variable@ is a @CombinerVariableNV@.
--
-- The parameter @input@ is a @CombinerRegisterNV@.
--
-- The parameter @mapping@ is a @CombinerMappingNV@.
--
-- The parameter @componentUsage@ is a @CombinerComponentUsageNV@.


glFinalCombinerInputNV :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> m ()
glFinalCombinerInputNV = ffienumenumenumenumIOV glFinalCombinerInputNVFunPtr

glFinalCombinerInputNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
glFinalCombinerInputNVFunPtr = unsafePerformIO (getProcAddress "glFinalCombinerInputNV")

{-# NOINLINE glFinalCombinerInputNVFunPtr #-}

-- | Usage: @'glGetCombinerInputParameterfvNV' stage portion variable pname params@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @portion@ is a @CombinerPortionNV@.
--
-- The parameter @variable@ is a @CombinerVariableNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetCombinerInputParameterfvNV :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetCombinerInputParameterfvNV = ffienumenumenumenumPtrfloatIOV glGetCombinerInputParameterfvNVFunPtr

glGetCombinerInputParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetCombinerInputParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetCombinerInputParameterfvNV")

{-# NOINLINE glGetCombinerInputParameterfvNVFunPtr #-}

-- | Usage: @'glGetCombinerInputParameterivNV' stage portion variable pname params@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @portion@ is a @CombinerPortionNV@.
--
-- The parameter @variable@ is a @CombinerVariableNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetCombinerInputParameterivNV :: MonadIO m => GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetCombinerInputParameterivNV = ffienumenumenumenumPtrintIOV glGetCombinerInputParameterivNVFunPtr

glGetCombinerInputParameterivNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetCombinerInputParameterivNVFunPtr = unsafePerformIO (getProcAddress "glGetCombinerInputParameterivNV")

{-# NOINLINE glGetCombinerInputParameterivNVFunPtr #-}

-- | Usage: @'glGetCombinerOutputParameterfvNV' stage portion pname params@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @portion@ is a @CombinerPortionNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetCombinerOutputParameterfvNV :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetCombinerOutputParameterfvNV = ffienumenumenumPtrfloatIOV glGetCombinerOutputParameterfvNVFunPtr

glGetCombinerOutputParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetCombinerOutputParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetCombinerOutputParameterfvNV")

{-# NOINLINE glGetCombinerOutputParameterfvNVFunPtr #-}

-- | Usage: @'glGetCombinerOutputParameterivNV' stage portion pname params@
--
-- The parameter @stage@ is a @CombinerStageNV@.
--
-- The parameter @portion@ is a @CombinerPortionNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetCombinerOutputParameterivNV :: MonadIO m => GLenum -> GLenum -> GLenum -> Ptr GLint -> m ()
glGetCombinerOutputParameterivNV = ffienumenumenumPtrintIOV glGetCombinerOutputParameterivNVFunPtr

glGetCombinerOutputParameterivNVFunPtr :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
glGetCombinerOutputParameterivNVFunPtr = unsafePerformIO (getProcAddress "glGetCombinerOutputParameterivNV")

{-# NOINLINE glGetCombinerOutputParameterivNVFunPtr #-}

-- | Usage: @'glGetFinalCombinerInputParameterfvNV' variable pname params@
--
-- The parameter @variable@ is a @CombinerVariableNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFinalCombinerInputParameterfvNV :: MonadIO m => GLenum -> GLenum -> Ptr GLfloat -> m ()
glGetFinalCombinerInputParameterfvNV = ffienumenumPtrfloatIOV glGetFinalCombinerInputParameterfvNVFunPtr

glGetFinalCombinerInputParameterfvNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
glGetFinalCombinerInputParameterfvNVFunPtr = unsafePerformIO (getProcAddress "glGetFinalCombinerInputParameterfvNV")

{-# NOINLINE glGetFinalCombinerInputParameterfvNVFunPtr #-}

-- | Usage: @'glGetFinalCombinerInputParameterivNV' variable pname params@
--
-- The parameter @variable@ is a @CombinerVariableNV@.
--
-- The parameter @pname@ is a @CombinerParameterNV@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetFinalCombinerInputParameterivNV :: MonadIO m => GLenum -> GLenum -> Ptr GLint -> m ()
glGetFinalCombinerInputParameterivNV = ffienumenumPtrintIOV glGetFinalCombinerInputParameterivNVFunPtr

glGetFinalCombinerInputParameterivNVFunPtr :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
glGetFinalCombinerInputParameterivNVFunPtr = unsafePerformIO (getProcAddress "glGetFinalCombinerInputParameterivNV")

{-# NOINLINE glGetFinalCombinerInputParameterivNVFunPtr #-}

pattern GL_BIAS_BY_NEGATIVE_ONE_HALF_NV  :: (Eq a, Num a) => a

pattern GL_BIAS_BY_NEGATIVE_ONE_HALF_NV = 0x8541

pattern GL_COLOR_SUM_CLAMP_NV  :: (Eq a, Num a) => a

pattern GL_COLOR_SUM_CLAMP_NV = 0x854F

pattern GL_COMBINER0_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER0_NV = 0x8550

pattern GL_COMBINER1_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER1_NV = 0x8551

pattern GL_COMBINER2_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER2_NV = 0x8552

pattern GL_COMBINER3_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER3_NV = 0x8553

pattern GL_COMBINER4_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER4_NV = 0x8554

pattern GL_COMBINER5_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER5_NV = 0x8555

pattern GL_COMBINER6_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER6_NV = 0x8556

pattern GL_COMBINER7_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER7_NV = 0x8557

pattern GL_COMBINER_AB_DOT_PRODUCT_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_AB_DOT_PRODUCT_NV = 0x8545

pattern GL_COMBINER_AB_OUTPUT_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_AB_OUTPUT_NV = 0x854A

pattern GL_COMBINER_BIAS_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_BIAS_NV = 0x8549

pattern GL_COMBINER_CD_DOT_PRODUCT_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_CD_DOT_PRODUCT_NV = 0x8546

pattern GL_COMBINER_CD_OUTPUT_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_CD_OUTPUT_NV = 0x854B

pattern GL_COMBINER_COMPONENT_USAGE_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_COMPONENT_USAGE_NV = 0x8544

pattern GL_COMBINER_INPUT_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_INPUT_NV = 0x8542

pattern GL_COMBINER_MAPPING_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_MAPPING_NV = 0x8543

pattern GL_COMBINER_MUX_SUM_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_MUX_SUM_NV = 0x8547

pattern GL_COMBINER_SCALE_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_SCALE_NV = 0x8548

pattern GL_COMBINER_SUM_OUTPUT_NV  :: (Eq a, Num a) => a

pattern GL_COMBINER_SUM_OUTPUT_NV = 0x854C

pattern GL_CONSTANT_COLOR0_NV  :: (Eq a, Num a) => a

pattern GL_CONSTANT_COLOR0_NV = 0x852A

pattern GL_CONSTANT_COLOR1_NV  :: (Eq a, Num a) => a

pattern GL_CONSTANT_COLOR1_NV = 0x852B

pattern GL_DISCARD_NV  :: (Eq a, Num a) => a

pattern GL_DISCARD_NV = 0x8530

pattern GL_EXPAND_NEGATE_NV  :: (Eq a, Num a) => a

pattern GL_EXPAND_NEGATE_NV = 0x8539

pattern GL_EXPAND_NORMAL_NV  :: (Eq a, Num a) => a

pattern GL_EXPAND_NORMAL_NV = 0x8538

pattern GL_E_TIMES_F_NV  :: (Eq a, Num a) => a

pattern GL_E_TIMES_F_NV = 0x8531

pattern GL_HALF_BIAS_NEGATE_NV  :: (Eq a, Num a) => a

pattern GL_HALF_BIAS_NEGATE_NV = 0x853B

pattern GL_HALF_BIAS_NORMAL_NV  :: (Eq a, Num a) => a

pattern GL_HALF_BIAS_NORMAL_NV = 0x853A

pattern GL_MAX_GENERAL_COMBINERS_NV  :: (Eq a, Num a) => a

pattern GL_MAX_GENERAL_COMBINERS_NV = 0x854D

pattern GL_NUM_GENERAL_COMBINERS_NV  :: (Eq a, Num a) => a

pattern GL_NUM_GENERAL_COMBINERS_NV = 0x854E

pattern GL_REGISTER_COMBINERS_NV  :: (Eq a, Num a) => a

pattern GL_REGISTER_COMBINERS_NV = 0x8522

pattern GL_SCALE_BY_FOUR_NV  :: (Eq a, Num a) => a

pattern GL_SCALE_BY_FOUR_NV = 0x853F

pattern GL_SCALE_BY_ONE_HALF_NV  :: (Eq a, Num a) => a

pattern GL_SCALE_BY_ONE_HALF_NV = 0x8540

pattern GL_SCALE_BY_TWO_NV  :: (Eq a, Num a) => a

pattern GL_SCALE_BY_TWO_NV = 0x853E

pattern GL_SIGNED_IDENTITY_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_IDENTITY_NV = 0x853C

pattern GL_SIGNED_NEGATE_NV  :: (Eq a, Num a) => a

pattern GL_SIGNED_NEGATE_NV = 0x853D

pattern GL_SPARE0_NV  :: (Eq a, Num a) => a

pattern GL_SPARE0_NV = 0x852E

pattern GL_SPARE0_PLUS_SECONDARY_COLOR_NV  :: (Eq a, Num a) => a

pattern GL_SPARE0_PLUS_SECONDARY_COLOR_NV = 0x8532

pattern GL_SPARE1_NV  :: (Eq a, Num a) => a

pattern GL_SPARE1_NV = 0x852F

pattern GL_UNSIGNED_IDENTITY_NV  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_IDENTITY_NV = 0x8536

pattern GL_UNSIGNED_INVERT_NV  :: (Eq a, Num a) => a

pattern GL_UNSIGNED_INVERT_NV = 0x8537

pattern GL_VARIABLE_A_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_A_NV = 0x8523

pattern GL_VARIABLE_B_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_B_NV = 0x8524

pattern GL_VARIABLE_C_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_C_NV = 0x8525

pattern GL_VARIABLE_D_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_D_NV = 0x8526

pattern GL_VARIABLE_E_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_E_NV = 0x8527

pattern GL_VARIABLE_F_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_F_NV = 0x8528

pattern GL_VARIABLE_G_NV  :: (Eq a, Num a) => a

pattern GL_VARIABLE_G_NV = 0x8529