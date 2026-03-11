-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.CommandList (
  -- * Extension Support
    gl_NV_command_list

  -- * GL_NV_command_list
  , glCallCommandListNV
  , glCommandListSegmentsNV
  , glCompileCommandListNV
  , glCreateCommandListsNV
  , glCreateStatesNV
  , glDeleteCommandListsNV
  , glDeleteStatesNV
  , glDrawCommandsAddressNV
  , glDrawCommandsNV
  , glDrawCommandsStatesAddressNV
  , glDrawCommandsStatesNV
  , glGetCommandHeaderNV
  , glGetStageIndexNV
  , glIsCommandListNV
  , glIsStateNV
  , glListDrawCommandsStatesClientNV
  , glStateCaptureNV
  , pattern GL_ALPHA_REF_COMMAND_NV
  , pattern GL_ATTRIBUTE_ADDRESS_COMMAND_NV
  , pattern GL_BLEND_COLOR_COMMAND_NV
  , pattern GL_DRAW_ARRAYS_COMMAND_NV
  , pattern GL_DRAW_ARRAYS_INSTANCED_COMMAND_NV
  , pattern GL_DRAW_ARRAYS_STRIP_COMMAND_NV
  , pattern GL_DRAW_ELEMENTS_COMMAND_NV
  , pattern GL_DRAW_ELEMENTS_INSTANCED_COMMAND_NV
  , pattern GL_DRAW_ELEMENTS_STRIP_COMMAND_NV
  , pattern GL_ELEMENT_ADDRESS_COMMAND_NV
  , pattern GL_FRONT_FACE_COMMAND_NV
  , pattern GL_LINE_WIDTH_COMMAND_NV
  , pattern GL_NOP_COMMAND_NV
  , pattern GL_POLYGON_OFFSET_COMMAND_NV
  , pattern GL_SCISSOR_COMMAND_NV
  , pattern GL_STENCIL_REF_COMMAND_NV
  , pattern GL_TERMINATE_SEQUENCE_COMMAND_NV
  , pattern GL_UNIFORM_ADDRESS_COMMAND_NV
  , pattern GL_VIEWPORT_COMMAND_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/command_list.txt GL_NV_command_list> extension is available.

gl_NV_command_list :: Bool
gl_NV_command_list = member "GL_NV_command_list" extensions
{-# NOINLINE gl_NV_command_list #-}

-- | Usage: @'glCallCommandListNV' list@


glCallCommandListNV :: MonadIO m => GLuint -> m ()
glCallCommandListNV = ffiuintIOV glCallCommandListNVFunPtr

glCallCommandListNVFunPtr :: FunPtr (GLuint -> IO ())
glCallCommandListNVFunPtr = unsafePerformIO (getProcAddress "glCallCommandListNV")

{-# NOINLINE glCallCommandListNVFunPtr #-}

-- | Usage: @'glCommandListSegmentsNV' list segments@


glCommandListSegmentsNV :: MonadIO m => GLuint -> GLuint -> m ()
glCommandListSegmentsNV = ffiuintuintIOV glCommandListSegmentsNVFunPtr

glCommandListSegmentsNVFunPtr :: FunPtr (GLuint -> GLuint -> IO ())
glCommandListSegmentsNVFunPtr = unsafePerformIO (getProcAddress "glCommandListSegmentsNV")

{-# NOINLINE glCommandListSegmentsNVFunPtr #-}

-- | Usage: @'glCompileCommandListNV' list@


glCompileCommandListNV :: MonadIO m => GLuint -> m ()
glCompileCommandListNV = ffiuintIOV glCompileCommandListNVFunPtr

glCompileCommandListNVFunPtr :: FunPtr (GLuint -> IO ())
glCompileCommandListNVFunPtr = unsafePerformIO (getProcAddress "glCompileCommandListNV")

{-# NOINLINE glCompileCommandListNVFunPtr #-}

-- | Usage: @'glCreateCommandListsNV' n lists@


glCreateCommandListsNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glCreateCommandListsNV = ffisizeiPtruintIOV glCreateCommandListsNVFunPtr

glCreateCommandListsNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glCreateCommandListsNVFunPtr = unsafePerformIO (getProcAddress "glCreateCommandListsNV")

{-# NOINLINE glCreateCommandListsNVFunPtr #-}

-- | Usage: @'glCreateStatesNV' n states@


glCreateStatesNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glCreateStatesNV = ffisizeiPtruintIOV glCreateStatesNVFunPtr

glCreateStatesNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glCreateStatesNVFunPtr = unsafePerformIO (getProcAddress "glCreateStatesNV")

{-# NOINLINE glCreateStatesNVFunPtr #-}

-- | Usage: @'glDeleteCommandListsNV' n lists@


glDeleteCommandListsNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteCommandListsNV = ffisizeiPtruintIOV glDeleteCommandListsNVFunPtr

glDeleteCommandListsNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteCommandListsNVFunPtr = unsafePerformIO (getProcAddress "glDeleteCommandListsNV")

{-# NOINLINE glDeleteCommandListsNVFunPtr #-}

-- | Usage: @'glDeleteStatesNV' n states@


glDeleteStatesNV :: MonadIO m => GLsizei -> Ptr GLuint -> m ()
glDeleteStatesNV = ffisizeiPtruintIOV glDeleteStatesNVFunPtr

glDeleteStatesNVFunPtr :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
glDeleteStatesNVFunPtr = unsafePerformIO (getProcAddress "glDeleteStatesNV")

{-# NOINLINE glDeleteStatesNVFunPtr #-}

-- | Usage: @'glDrawCommandsAddressNV' primitiveMode indirects sizes count@


glDrawCommandsAddressNV :: MonadIO m => GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> m ()
glDrawCommandsAddressNV = ffienumPtruint64PtrsizeiuintIOV glDrawCommandsAddressNVFunPtr

glDrawCommandsAddressNVFunPtr :: FunPtr (GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> IO ())
glDrawCommandsAddressNVFunPtr = unsafePerformIO (getProcAddress "glDrawCommandsAddressNV")

{-# NOINLINE glDrawCommandsAddressNVFunPtr #-}

-- | Usage: @'glDrawCommandsNV' primitiveMode buffer indirects sizes count@


glDrawCommandsNV :: MonadIO m => GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> m ()
glDrawCommandsNV = ffienumuintPtrintptrPtrsizeiuintIOV glDrawCommandsNVFunPtr

glDrawCommandsNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> IO ())
glDrawCommandsNVFunPtr = unsafePerformIO (getProcAddress "glDrawCommandsNV")

{-# NOINLINE glDrawCommandsNVFunPtr #-}

-- | Usage: @'glDrawCommandsStatesAddressNV' indirects sizes states fbos count@


glDrawCommandsStatesAddressNV :: MonadIO m => Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> m ()
glDrawCommandsStatesAddressNV = ffiPtruint64PtrsizeiPtruintPtruintuintIOV glDrawCommandsStatesAddressNVFunPtr

glDrawCommandsStatesAddressNVFunPtr :: FunPtr (Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
glDrawCommandsStatesAddressNVFunPtr = unsafePerformIO (getProcAddress "glDrawCommandsStatesAddressNV")

{-# NOINLINE glDrawCommandsStatesAddressNVFunPtr #-}

-- | Usage: @'glDrawCommandsStatesNV' buffer indirects sizes states fbos count@


glDrawCommandsStatesNV :: MonadIO m => GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> m ()
glDrawCommandsStatesNV = ffiuintPtrintptrPtrsizeiPtruintPtruintuintIOV glDrawCommandsStatesNVFunPtr

glDrawCommandsStatesNVFunPtr :: FunPtr (GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
glDrawCommandsStatesNVFunPtr = unsafePerformIO (getProcAddress "glDrawCommandsStatesNV")

{-# NOINLINE glDrawCommandsStatesNVFunPtr #-}

-- | Usage: @'glGetCommandHeaderNV' tokenID size@


glGetCommandHeaderNV :: MonadIO m => GLenum -> GLuint -> m GLuint
glGetCommandHeaderNV = ffienumuintIOuint glGetCommandHeaderNVFunPtr

glGetCommandHeaderNVFunPtr :: FunPtr (GLenum -> GLuint -> IO GLuint)
glGetCommandHeaderNVFunPtr = unsafePerformIO (getProcAddress "glGetCommandHeaderNV")

{-# NOINLINE glGetCommandHeaderNVFunPtr #-}

-- | Usage: @'glGetStageIndexNV' shadertype@


glGetStageIndexNV :: MonadIO m => GLenum -> m GLushort
glGetStageIndexNV = ffienumIOushort glGetStageIndexNVFunPtr

glGetStageIndexNVFunPtr :: FunPtr (GLenum -> IO GLushort)
glGetStageIndexNVFunPtr = unsafePerformIO (getProcAddress "glGetStageIndexNV")

{-# NOINLINE glGetStageIndexNVFunPtr #-}

-- | Usage: @'glIsCommandListNV' list@


glIsCommandListNV :: MonadIO m => GLuint -> m GLboolean
glIsCommandListNV = ffiuintIOboolean glIsCommandListNVFunPtr

glIsCommandListNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsCommandListNVFunPtr = unsafePerformIO (getProcAddress "glIsCommandListNV")

{-# NOINLINE glIsCommandListNVFunPtr #-}

-- | Usage: @'glIsStateNV' state@


glIsStateNV :: MonadIO m => GLuint -> m GLboolean
glIsStateNV = ffiuintIOboolean glIsStateNVFunPtr

glIsStateNVFunPtr :: FunPtr (GLuint -> IO GLboolean)
glIsStateNVFunPtr = unsafePerformIO (getProcAddress "glIsStateNV")

{-# NOINLINE glIsStateNVFunPtr #-}

-- | Usage: @'glListDrawCommandsStatesClientNV' list segment indirects sizes states fbos count@


glListDrawCommandsStatesClientNV :: MonadIO m => GLuint -> GLuint -> Ptr (Ptr ()) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> m ()
glListDrawCommandsStatesClientNV = ffiuintuintPtrPtrVPtrsizeiPtruintPtruintuintIOV glListDrawCommandsStatesClientNVFunPtr

glListDrawCommandsStatesClientNVFunPtr :: FunPtr (GLuint -> GLuint -> Ptr (Ptr ()) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
glListDrawCommandsStatesClientNVFunPtr = unsafePerformIO (getProcAddress "glListDrawCommandsStatesClientNV")

{-# NOINLINE glListDrawCommandsStatesClientNVFunPtr #-}

-- | Usage: @'glStateCaptureNV' state mode@


glStateCaptureNV :: MonadIO m => GLuint -> GLenum -> m ()
glStateCaptureNV = ffiuintenumIOV glStateCaptureNVFunPtr

glStateCaptureNVFunPtr :: FunPtr (GLuint -> GLenum -> IO ())
glStateCaptureNVFunPtr = unsafePerformIO (getProcAddress "glStateCaptureNV")

{-# NOINLINE glStateCaptureNVFunPtr #-}

pattern GL_ALPHA_REF_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_ALPHA_REF_COMMAND_NV = 0x000F

pattern GL_ATTRIBUTE_ADDRESS_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_ATTRIBUTE_ADDRESS_COMMAND_NV = 0x0009

pattern GL_BLEND_COLOR_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_BLEND_COLOR_COMMAND_NV = 0x000B

pattern GL_DRAW_ARRAYS_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_ARRAYS_COMMAND_NV = 0x0003

pattern GL_DRAW_ARRAYS_INSTANCED_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_ARRAYS_INSTANCED_COMMAND_NV = 0x0007

pattern GL_DRAW_ARRAYS_STRIP_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_ARRAYS_STRIP_COMMAND_NV = 0x0005

pattern GL_DRAW_ELEMENTS_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_ELEMENTS_COMMAND_NV = 0x0002

pattern GL_DRAW_ELEMENTS_INSTANCED_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_ELEMENTS_INSTANCED_COMMAND_NV = 0x0006

pattern GL_DRAW_ELEMENTS_STRIP_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_ELEMENTS_STRIP_COMMAND_NV = 0x0004

pattern GL_ELEMENT_ADDRESS_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ADDRESS_COMMAND_NV = 0x0008

pattern GL_FRONT_FACE_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_FRONT_FACE_COMMAND_NV = 0x0012

pattern GL_LINE_WIDTH_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_LINE_WIDTH_COMMAND_NV = 0x000D

pattern GL_NOP_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_NOP_COMMAND_NV = 0x0001

pattern GL_POLYGON_OFFSET_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_POLYGON_OFFSET_COMMAND_NV = 0x000E

pattern GL_SCISSOR_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_SCISSOR_COMMAND_NV = 0x0011

pattern GL_STENCIL_REF_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_STENCIL_REF_COMMAND_NV = 0x000C

pattern GL_TERMINATE_SEQUENCE_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_TERMINATE_SEQUENCE_COMMAND_NV = 0x0000

pattern GL_UNIFORM_ADDRESS_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_UNIFORM_ADDRESS_COMMAND_NV = 0x000A

pattern GL_VIEWPORT_COMMAND_NV  :: (Eq a, Num a) => a

pattern GL_VIEWPORT_COMMAND_NV = 0x0010