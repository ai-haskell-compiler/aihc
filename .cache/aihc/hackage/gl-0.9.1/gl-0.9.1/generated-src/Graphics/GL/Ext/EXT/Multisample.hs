-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.Multisample (
  -- * Extension Support
    gl_EXT_multisample

  -- * GL_EXT_multisample
  , glSampleMaskEXT
  , glSamplePatternEXT
  , pattern GL_1PASS_EXT
  , pattern GL_2PASS_0_EXT
  , pattern GL_2PASS_1_EXT
  , pattern GL_4PASS_0_EXT
  , pattern GL_4PASS_1_EXT
  , pattern GL_4PASS_2_EXT
  , pattern GL_4PASS_3_EXT
  , pattern GL_MULTISAMPLE_BIT_EXT
  , pattern GL_MULTISAMPLE_EXT
  , pattern GL_SAMPLES_EXT
  , pattern GL_SAMPLE_ALPHA_TO_MASK_EXT
  , pattern GL_SAMPLE_ALPHA_TO_ONE_EXT
  , pattern GL_SAMPLE_BUFFERS_EXT
  , pattern GL_SAMPLE_MASK_EXT
  , pattern GL_SAMPLE_MASK_INVERT_EXT
  , pattern GL_SAMPLE_MASK_VALUE_EXT
  , pattern GL_SAMPLE_PATTERN_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the GL_EXT_multisample extension is available.

gl_EXT_multisample :: Bool
gl_EXT_multisample = member "GL_EXT_multisample" extensions
{-# NOINLINE gl_EXT_multisample #-}

-- | Usage: @'glSampleMaskEXT' value invert@
--
-- The parameter @value@ is a @ClampedFloat32@.
--
-- The parameter @invert@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glSampleMaskEXT :: MonadIO m => GLclampf -> GLboolean -> m ()
glSampleMaskEXT = fficlampfbooleanIOV glSampleMaskEXTFunPtr

glSampleMaskEXTFunPtr :: FunPtr (GLclampf -> GLboolean -> IO ())
glSampleMaskEXTFunPtr = unsafePerformIO (getProcAddress "glSampleMaskEXT")

{-# NOINLINE glSampleMaskEXTFunPtr #-}

-- | Usage: @'glSamplePatternEXT' pattern@
--
-- The parameter @pattern@ is a @SamplePatternEXT@.


glSamplePatternEXT :: MonadIO m => GLenum -> m ()
glSamplePatternEXT = ffienumIOV glSamplePatternEXTFunPtr

glSamplePatternEXTFunPtr :: FunPtr (GLenum -> IO ())
glSamplePatternEXTFunPtr = unsafePerformIO (getProcAddress "glSamplePatternEXT")

{-# NOINLINE glSamplePatternEXTFunPtr #-}

pattern GL_1PASS_EXT  :: (Eq a, Num a) => a

pattern GL_1PASS_EXT = 0x80A1

pattern GL_2PASS_0_EXT  :: (Eq a, Num a) => a

pattern GL_2PASS_0_EXT = 0x80A2

pattern GL_2PASS_1_EXT  :: (Eq a, Num a) => a

pattern GL_2PASS_1_EXT = 0x80A3

pattern GL_4PASS_0_EXT  :: (Eq a, Num a) => a

pattern GL_4PASS_0_EXT = 0x80A4

pattern GL_4PASS_1_EXT  :: (Eq a, Num a) => a

pattern GL_4PASS_1_EXT = 0x80A5

pattern GL_4PASS_2_EXT  :: (Eq a, Num a) => a

pattern GL_4PASS_2_EXT = 0x80A6

pattern GL_4PASS_3_EXT  :: (Eq a, Num a) => a

pattern GL_4PASS_3_EXT = 0x80A7

pattern GL_MULTISAMPLE_BIT_EXT  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BIT_EXT = 0x20000000

pattern GL_SAMPLES_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLES_EXT = 0x80A9

pattern GL_SAMPLE_ALPHA_TO_MASK_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLE_ALPHA_TO_MASK_EXT = 0x809E

pattern GL_SAMPLE_BUFFERS_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLE_BUFFERS_EXT = 0x80A8

pattern GL_SAMPLE_MASK_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_EXT = 0x80A0

pattern GL_SAMPLE_MASK_INVERT_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_INVERT_EXT = 0x80AB

pattern GL_SAMPLE_MASK_VALUE_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLE_MASK_VALUE_EXT = 0x80AA

pattern GL_SAMPLE_PATTERN_EXT  :: (Eq a, Num a) => a

pattern GL_SAMPLE_PATTERN_EXT = 0x80AC