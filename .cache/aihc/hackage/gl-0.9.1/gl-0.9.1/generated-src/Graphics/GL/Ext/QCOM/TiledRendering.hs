-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.QCOM.TiledRendering (
  -- * Extension Support
    gl_QCOM_tiled_rendering

  -- * GL_QCOM_tiled_rendering
  , glEndTilingQCOM
  , glStartTilingQCOM
  , pattern GL_COLOR_BUFFER_BIT0_QCOM
  , pattern GL_COLOR_BUFFER_BIT1_QCOM
  , pattern GL_COLOR_BUFFER_BIT2_QCOM
  , pattern GL_COLOR_BUFFER_BIT3_QCOM
  , pattern GL_COLOR_BUFFER_BIT4_QCOM
  , pattern GL_COLOR_BUFFER_BIT5_QCOM
  , pattern GL_COLOR_BUFFER_BIT6_QCOM
  , pattern GL_COLOR_BUFFER_BIT7_QCOM
  , pattern GL_DEPTH_BUFFER_BIT0_QCOM
  , pattern GL_DEPTH_BUFFER_BIT1_QCOM
  , pattern GL_DEPTH_BUFFER_BIT2_QCOM
  , pattern GL_DEPTH_BUFFER_BIT3_QCOM
  , pattern GL_DEPTH_BUFFER_BIT4_QCOM
  , pattern GL_DEPTH_BUFFER_BIT5_QCOM
  , pattern GL_DEPTH_BUFFER_BIT6_QCOM
  , pattern GL_DEPTH_BUFFER_BIT7_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT0_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT1_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT2_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT3_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT4_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT5_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT6_QCOM
  , pattern GL_MULTISAMPLE_BUFFER_BIT7_QCOM
  , pattern GL_STENCIL_BUFFER_BIT0_QCOM
  , pattern GL_STENCIL_BUFFER_BIT1_QCOM
  , pattern GL_STENCIL_BUFFER_BIT2_QCOM
  , pattern GL_STENCIL_BUFFER_BIT3_QCOM
  , pattern GL_STENCIL_BUFFER_BIT4_QCOM
  , pattern GL_STENCIL_BUFFER_BIT5_QCOM
  , pattern GL_STENCIL_BUFFER_BIT6_QCOM
  , pattern GL_STENCIL_BUFFER_BIT7_QCOM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/QCOM/QCOM_tiled_rendering.txt GL_QCOM_tiled_rendering> extension is available.

gl_QCOM_tiled_rendering :: Bool
gl_QCOM_tiled_rendering = member "GL_QCOM_tiled_rendering" extensions
{-# NOINLINE gl_QCOM_tiled_rendering #-}

-- | Usage: @'glEndTilingQCOM' preserveMask@


glEndTilingQCOM :: MonadIO m => GLbitfield -> m ()
glEndTilingQCOM = ffibitfieldIOV glEndTilingQCOMFunPtr

glEndTilingQCOMFunPtr :: FunPtr (GLbitfield -> IO ())
glEndTilingQCOMFunPtr = unsafePerformIO (getProcAddress "glEndTilingQCOM")

{-# NOINLINE glEndTilingQCOMFunPtr #-}

-- | Usage: @'glStartTilingQCOM' x y width height preserveMask@


glStartTilingQCOM :: MonadIO m => GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> m ()
glStartTilingQCOM = ffiuintuintuintuintbitfieldIOV glStartTilingQCOMFunPtr

glStartTilingQCOMFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> IO ())
glStartTilingQCOMFunPtr = unsafePerformIO (getProcAddress "glStartTilingQCOM")

{-# NOINLINE glStartTilingQCOMFunPtr #-}

pattern GL_COLOR_BUFFER_BIT0_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT0_QCOM = 0x00000001

pattern GL_COLOR_BUFFER_BIT1_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT1_QCOM = 0x00000002

pattern GL_COLOR_BUFFER_BIT2_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT2_QCOM = 0x00000004

pattern GL_COLOR_BUFFER_BIT3_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT3_QCOM = 0x00000008

pattern GL_COLOR_BUFFER_BIT4_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT4_QCOM = 0x00000010

pattern GL_COLOR_BUFFER_BIT5_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT5_QCOM = 0x00000020

pattern GL_COLOR_BUFFER_BIT6_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT6_QCOM = 0x00000040

pattern GL_COLOR_BUFFER_BIT7_QCOM  :: (Eq a, Num a) => a

pattern GL_COLOR_BUFFER_BIT7_QCOM = 0x00000080

pattern GL_DEPTH_BUFFER_BIT0_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT0_QCOM = 0x00000100

pattern GL_DEPTH_BUFFER_BIT1_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT1_QCOM = 0x00000200

pattern GL_DEPTH_BUFFER_BIT2_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT2_QCOM = 0x00000400

pattern GL_DEPTH_BUFFER_BIT3_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT3_QCOM = 0x00000800

pattern GL_DEPTH_BUFFER_BIT4_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT4_QCOM = 0x00001000

pattern GL_DEPTH_BUFFER_BIT5_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT5_QCOM = 0x00002000

pattern GL_DEPTH_BUFFER_BIT6_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT6_QCOM = 0x00004000

pattern GL_DEPTH_BUFFER_BIT7_QCOM  :: (Eq a, Num a) => a

pattern GL_DEPTH_BUFFER_BIT7_QCOM = 0x00008000

pattern GL_MULTISAMPLE_BUFFER_BIT0_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT0_QCOM = 0x01000000

pattern GL_MULTISAMPLE_BUFFER_BIT1_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT1_QCOM = 0x02000000

pattern GL_MULTISAMPLE_BUFFER_BIT2_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT2_QCOM = 0x04000000

pattern GL_MULTISAMPLE_BUFFER_BIT3_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT3_QCOM = 0x08000000

pattern GL_MULTISAMPLE_BUFFER_BIT4_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT4_QCOM = 0x10000000

pattern GL_MULTISAMPLE_BUFFER_BIT5_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT5_QCOM = 0x20000000

pattern GL_MULTISAMPLE_BUFFER_BIT6_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT6_QCOM = 0x40000000

pattern GL_MULTISAMPLE_BUFFER_BIT7_QCOM  :: (Eq a, Num a) => a

pattern GL_MULTISAMPLE_BUFFER_BIT7_QCOM = 0x80000000

pattern GL_STENCIL_BUFFER_BIT0_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT0_QCOM = 0x00010000

pattern GL_STENCIL_BUFFER_BIT1_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT1_QCOM = 0x00020000

pattern GL_STENCIL_BUFFER_BIT2_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT2_QCOM = 0x00040000

pattern GL_STENCIL_BUFFER_BIT3_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT3_QCOM = 0x00080000

pattern GL_STENCIL_BUFFER_BIT4_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT4_QCOM = 0x00100000

pattern GL_STENCIL_BUFFER_BIT5_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT5_QCOM = 0x00200000

pattern GL_STENCIL_BUFFER_BIT6_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT6_QCOM = 0x00400000

pattern GL_STENCIL_BUFFER_BIT7_QCOM  :: (Eq a, Num a) => a

pattern GL_STENCIL_BUFFER_BIT7_QCOM = 0x00800000