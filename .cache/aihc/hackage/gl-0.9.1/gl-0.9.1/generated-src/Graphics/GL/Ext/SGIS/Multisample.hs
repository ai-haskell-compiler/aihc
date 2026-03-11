-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIS.Multisample (
  -- * Extension Support
    gl_SGIS_multisample

  -- * GL_SGIS_multisample
  , glSampleMaskSGIS
  , glSamplePatternSGIS
  , pattern GL_1PASS_SGIS
  , pattern GL_2PASS_0_SGIS
  , pattern GL_2PASS_1_SGIS
  , pattern GL_4PASS_0_SGIS
  , pattern GL_4PASS_1_SGIS
  , pattern GL_4PASS_2_SGIS
  , pattern GL_4PASS_3_SGIS
  , pattern GL_MULTISAMPLE_SGIS
  , pattern GL_SAMPLES_SGIS
  , pattern GL_SAMPLE_ALPHA_TO_MASK_SGIS
  , pattern GL_SAMPLE_ALPHA_TO_ONE_SGIS
  , pattern GL_SAMPLE_BUFFERS_SGIS
  , pattern GL_SAMPLE_MASK_INVERT_SGIS
  , pattern GL_SAMPLE_MASK_SGIS
  , pattern GL_SAMPLE_MASK_VALUE_SGIS
  , pattern GL_SAMPLE_PATTERN_SGIS
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIS/multisample.txt GL_SGIS_multisample> extension is available.

gl_SGIS_multisample :: Bool
gl_SGIS_multisample = member "GL_SGIS_multisample" extensions
{-# NOINLINE gl_SGIS_multisample #-}

-- | Usage: @'glSampleMaskSGIS' value invert@
--
-- The parameter @value@ is a @ClampedFloat32@.
--
-- The parameter @invert@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.
--
-- This command is an alias for 'Graphics.GL.Ext.EXT.Multisample.glSampleMaskEXT'.


glSampleMaskSGIS :: MonadIO m => GLclampf -> GLboolean -> m ()
glSampleMaskSGIS = fficlampfbooleanIOV glSampleMaskSGISFunPtr

glSampleMaskSGISFunPtr :: FunPtr (GLclampf -> GLboolean -> IO ())
glSampleMaskSGISFunPtr = unsafePerformIO (getProcAddress "glSampleMaskSGIS")

{-# NOINLINE glSampleMaskSGISFunPtr #-}

-- | Usage: @'glSamplePatternSGIS' pattern@
--
-- The parameter @pattern@ is a @SamplePatternSGIS@, one of: 'Graphics.GL.Ext.EXT.Multisample.GL_1PASS_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_1PASS_SGIS', 'Graphics.GL.Ext.EXT.Multisample.GL_2PASS_0_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_2PASS_0_SGIS', 'Graphics.GL.Ext.EXT.Multisample.GL_2PASS_1_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_2PASS_1_SGIS', 'Graphics.GL.Ext.EXT.Multisample.GL_4PASS_0_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_4PASS_0_SGIS', 'Graphics.GL.Ext.EXT.Multisample.GL_4PASS_1_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_4PASS_1_SGIS', 'Graphics.GL.Ext.EXT.Multisample.GL_4PASS_2_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_4PASS_2_SGIS', 'Graphics.GL.Ext.EXT.Multisample.GL_4PASS_3_EXT', 'Graphics.GL.Ext.SGIS.Multisample.GL_4PASS_3_SGIS'.
--
-- This command is an alias for 'Graphics.GL.Ext.EXT.Multisample.glSamplePatternEXT'.


glSamplePatternSGIS :: MonadIO m => GLenum -> m ()
glSamplePatternSGIS = ffienumIOV glSamplePatternSGISFunPtr

glSamplePatternSGISFunPtr :: FunPtr (GLenum -> IO ())
glSamplePatternSGISFunPtr = unsafePerformIO (getProcAddress "glSamplePatternSGIS")

{-# NOINLINE glSamplePatternSGISFunPtr #-}

pattern GL_1PASS_SGIS  :: (Eq a, Num a) => a

pattern GL_1PASS_SGIS = 0x80A1

pattern GL_2PASS_0_SGIS  :: (Eq a, Num a) => a

pattern GL_2PASS_0_SGIS = 0x80A2

pattern GL_2PASS_1_SGIS  :: (Eq a, Num a) => a

pattern GL_2PASS_1_SGIS = 0x80A3

pattern GL_4PASS_0_SGIS  :: (Eq a, Num a) => a

pattern GL_4PASS_0_SGIS = 0x80A4

pattern GL_4PASS_1_SGIS  :: (Eq a, Num a) => a

pattern GL_4PASS_1_SGIS = 0x80A5

pattern GL_4PASS_2_SGIS  :: (Eq a, Num a) => a

pattern GL_4PASS_2_SGIS = 0x80A6

pattern GL_4PASS_3_SGIS  :: (Eq a, Num a) => a

pattern GL_4PASS_3_SGIS = 0x80A7

pattern GL_MULTISAMPLE_SGIS  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_SGIS = 0x809D

pattern GL_SAMPLES_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLES_SGIS = 0x80A9

pattern GL_SAMPLE_ALPHA_TO_MASK_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_ALPHA_TO_MASK_SGIS = 0x809E

pattern GL_SAMPLE_ALPHA_TO_ONE_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_ALPHA_TO_ONE_SGIS = 0x809F

pattern GL_SAMPLE_BUFFERS_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_BUFFERS_SGIS = 0x80A8

pattern GL_SAMPLE_MASK_INVERT_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_INVERT_SGIS = 0x80AB

pattern GL_SAMPLE_MASK_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_SGIS = 0x80A0

pattern GL_SAMPLE_MASK_VALUE_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_VALUE_SGIS = 0x80AA

pattern GL_SAMPLE_PATTERN_SGIS  :: (Eq a, Num a) => a

pattern GL_SAMPLE_PATTERN_SGIS = 0x80AC