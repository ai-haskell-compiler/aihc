-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.NV.VertexBufferUnifiedMemory (
  -- * Extension Support
    gl_NV_vertex_buffer_unified_memory

  -- * GL_NV_vertex_buffer_unified_memory
  , glBufferAddressRangeNV
  , glColorFormatNV
  , glEdgeFlagFormatNV
  , glFogCoordFormatNV
  , glGetIntegerui64i_vNV
  , glIndexFormatNV
  , glNormalFormatNV
  , glSecondaryColorFormatNV
  , glTexCoordFormatNV
  , glVertexAttribFormatNV
  , glVertexAttribIFormatNV
  , glVertexFormatNV
  , pattern GL_COLOR_ARRAY_ADDRESS_NV
  , pattern GL_COLOR_ARRAY_LENGTH_NV
  , pattern GL_DRAW_INDIRECT_ADDRESS_NV
  , pattern GL_DRAW_INDIRECT_LENGTH_NV
  , pattern GL_DRAW_INDIRECT_UNIFIED_NV
  , pattern GL_EDGE_FLAG_ARRAY_ADDRESS_NV
  , pattern GL_EDGE_FLAG_ARRAY_LENGTH_NV
  , pattern GL_ELEMENT_ARRAY_ADDRESS_NV
  , pattern GL_ELEMENT_ARRAY_LENGTH_NV
  , pattern GL_ELEMENT_ARRAY_UNIFIED_NV
  , pattern GL_FOG_COORD_ARRAY_ADDRESS_NV
  , pattern GL_FOG_COORD_ARRAY_LENGTH_NV
  , pattern GL_INDEX_ARRAY_ADDRESS_NV
  , pattern GL_INDEX_ARRAY_LENGTH_NV
  , pattern GL_NORMAL_ARRAY_ADDRESS_NV
  , pattern GL_NORMAL_ARRAY_LENGTH_NV
  , pattern GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV
  , pattern GL_SECONDARY_COLOR_ARRAY_LENGTH_NV
  , pattern GL_TEXTURE_COORD_ARRAY_ADDRESS_NV
  , pattern GL_TEXTURE_COORD_ARRAY_LENGTH_NV
  , pattern GL_VERTEX_ARRAY_ADDRESS_NV
  , pattern GL_VERTEX_ARRAY_LENGTH_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV
  , pattern GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/NV/vertex_buffer_unified_memory.txt GL_NV_vertex_buffer_unified_memory> extension is available.

gl_NV_vertex_buffer_unified_memory :: Bool
gl_NV_vertex_buffer_unified_memory = member "GL_NV_vertex_buffer_unified_memory" extensions
{-# NOINLINE gl_NV_vertex_buffer_unified_memory #-}

-- | Usage: @'glBufferAddressRangeNV' pname index address length@
--
-- The parameter @length@ is a @BufferSize@.


glBufferAddressRangeNV :: MonadIO m => GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> m ()
glBufferAddressRangeNV = ffienumuintuint64EXTsizeiptrIOV glBufferAddressRangeNVFunPtr

glBufferAddressRangeNVFunPtr :: FunPtr (GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> IO ())
glBufferAddressRangeNVFunPtr = unsafePerformIO (getProcAddress "glBufferAddressRangeNV")

{-# NOINLINE glBufferAddressRangeNVFunPtr #-}

-- | Usage: @'glColorFormatNV' size type stride@


glColorFormatNV :: MonadIO m => GLint -> GLenum -> GLsizei -> m ()
glColorFormatNV = ffiintenumsizeiIOV glColorFormatNVFunPtr

glColorFormatNVFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
glColorFormatNVFunPtr = unsafePerformIO (getProcAddress "glColorFormatNV")

{-# NOINLINE glColorFormatNVFunPtr #-}

-- | Usage: @'glEdgeFlagFormatNV' stride@


glEdgeFlagFormatNV :: MonadIO m => GLsizei -> m ()
glEdgeFlagFormatNV = ffisizeiIOV glEdgeFlagFormatNVFunPtr

glEdgeFlagFormatNVFunPtr :: FunPtr (GLsizei -> IO ())
glEdgeFlagFormatNVFunPtr = unsafePerformIO (getProcAddress "glEdgeFlagFormatNV")

{-# NOINLINE glEdgeFlagFormatNVFunPtr #-}

-- | Usage: @'glFogCoordFormatNV' type stride@


glFogCoordFormatNV :: MonadIO m => GLenum -> GLsizei -> m ()
glFogCoordFormatNV = ffienumsizeiIOV glFogCoordFormatNVFunPtr

glFogCoordFormatNVFunPtr :: FunPtr (GLenum -> GLsizei -> IO ())
glFogCoordFormatNVFunPtr = unsafePerformIO (getProcAddress "glFogCoordFormatNV")

{-# NOINLINE glFogCoordFormatNVFunPtr #-}

-- | Usage: @'glGetIntegerui64i_vNV' value index result@
--
-- The length of @result@ should be @COMPSIZE(value)@.


glGetIntegerui64i_vNV :: MonadIO m => GLenum -> GLuint -> Ptr GLuint64EXT -> m ()
glGetIntegerui64i_vNV = ffienumuintPtruint64EXTIOV glGetIntegerui64i_vNVFunPtr

glGetIntegerui64i_vNVFunPtr :: FunPtr (GLenum -> GLuint -> Ptr GLuint64EXT -> IO ())
glGetIntegerui64i_vNVFunPtr = unsafePerformIO (getProcAddress "glGetIntegerui64i_vNV")

{-# NOINLINE glGetIntegerui64i_vNVFunPtr #-}

-- | Usage: @'glIndexFormatNV' type stride@


glIndexFormatNV :: MonadIO m => GLenum -> GLsizei -> m ()
glIndexFormatNV = ffienumsizeiIOV glIndexFormatNVFunPtr

glIndexFormatNVFunPtr :: FunPtr (GLenum -> GLsizei -> IO ())
glIndexFormatNVFunPtr = unsafePerformIO (getProcAddress "glIndexFormatNV")

{-# NOINLINE glIndexFormatNVFunPtr #-}

-- | Usage: @'glNormalFormatNV' type stride@


glNormalFormatNV :: MonadIO m => GLenum -> GLsizei -> m ()
glNormalFormatNV = ffienumsizeiIOV glNormalFormatNVFunPtr

glNormalFormatNVFunPtr :: FunPtr (GLenum -> GLsizei -> IO ())
glNormalFormatNVFunPtr = unsafePerformIO (getProcAddress "glNormalFormatNV")

{-# NOINLINE glNormalFormatNVFunPtr #-}

-- | Usage: @'glSecondaryColorFormatNV' size type stride@


glSecondaryColorFormatNV :: MonadIO m => GLint -> GLenum -> GLsizei -> m ()
glSecondaryColorFormatNV = ffiintenumsizeiIOV glSecondaryColorFormatNVFunPtr

glSecondaryColorFormatNVFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
glSecondaryColorFormatNVFunPtr = unsafePerformIO (getProcAddress "glSecondaryColorFormatNV")

{-# NOINLINE glSecondaryColorFormatNVFunPtr #-}

-- | Usage: @'glTexCoordFormatNV' size type stride@


glTexCoordFormatNV :: MonadIO m => GLint -> GLenum -> GLsizei -> m ()
glTexCoordFormatNV = ffiintenumsizeiIOV glTexCoordFormatNVFunPtr

glTexCoordFormatNVFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
glTexCoordFormatNVFunPtr = unsafePerformIO (getProcAddress "glTexCoordFormatNV")

{-# NOINLINE glTexCoordFormatNVFunPtr #-}

-- | Usage: @'glVertexAttribFormatNV' index size type normalized stride@
--
-- The parameter @normalized@ is a @Boolean@, one of: 'Graphics.GL.Internal.Shared.GL_FALSE', 'Graphics.GL.Internal.Shared.GL_TRUE'.


glVertexAttribFormatNV :: MonadIO m => GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> m ()
glVertexAttribFormatNV = ffiuintintenumbooleansizeiIOV glVertexAttribFormatNVFunPtr

glVertexAttribFormatNVFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> IO ())
glVertexAttribFormatNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribFormatNV")

{-# NOINLINE glVertexAttribFormatNVFunPtr #-}

-- | Usage: @'glVertexAttribIFormatNV' index size type stride@


glVertexAttribIFormatNV :: MonadIO m => GLuint -> GLint -> GLenum -> GLsizei -> m ()
glVertexAttribIFormatNV = ffiuintintenumsizeiIOV glVertexAttribIFormatNVFunPtr

glVertexAttribIFormatNVFunPtr :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ())
glVertexAttribIFormatNVFunPtr = unsafePerformIO (getProcAddress "glVertexAttribIFormatNV")

{-# NOINLINE glVertexAttribIFormatNVFunPtr #-}

-- | Usage: @'glVertexFormatNV' size type stride@


glVertexFormatNV :: MonadIO m => GLint -> GLenum -> GLsizei -> m ()
glVertexFormatNV = ffiintenumsizeiIOV glVertexFormatNVFunPtr

glVertexFormatNVFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
glVertexFormatNVFunPtr = unsafePerformIO (getProcAddress "glVertexFormatNV")

{-# NOINLINE glVertexFormatNVFunPtr #-}

pattern GL_COLOR_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_ADDRESS_NV = 0x8F23

pattern GL_COLOR_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_LENGTH_NV = 0x8F2D

pattern GL_DRAW_INDIRECT_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_INDIRECT_ADDRESS_NV = 0x8F41

pattern GL_DRAW_INDIRECT_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_INDIRECT_LENGTH_NV = 0x8F42

pattern GL_DRAW_INDIRECT_UNIFIED_NV  :: (Eq a, Num a) => a

pattern GL_DRAW_INDIRECT_UNIFIED_NV = 0x8F40

pattern GL_EDGE_FLAG_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_ADDRESS_NV = 0x8F26

pattern GL_EDGE_FLAG_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_LENGTH_NV = 0x8F30

pattern GL_ELEMENT_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_ADDRESS_NV = 0x8F29

pattern GL_ELEMENT_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_LENGTH_NV = 0x8F33

pattern GL_ELEMENT_ARRAY_UNIFIED_NV  :: (Eq a, Num a) => a

pattern GL_ELEMENT_ARRAY_UNIFIED_NV = 0x8F1F

pattern GL_FOG_COORD_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_FOG_COORD_ARRAY_ADDRESS_NV = 0x8F28

pattern GL_FOG_COORD_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_FOG_COORD_ARRAY_LENGTH_NV = 0x8F32

pattern GL_INDEX_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_ADDRESS_NV = 0x8F24

pattern GL_INDEX_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_LENGTH_NV = 0x8F2E

pattern GL_NORMAL_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_ADDRESS_NV = 0x8F22

pattern GL_NORMAL_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_LENGTH_NV = 0x8F2C

pattern GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_ADDRESS_NV = 0x8F27

pattern GL_SECONDARY_COLOR_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_LENGTH_NV = 0x8F31

pattern GL_TEXTURE_COORD_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_ADDRESS_NV = 0x8F25

pattern GL_TEXTURE_COORD_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_LENGTH_NV = 0x8F2F

pattern GL_VERTEX_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_ADDRESS_NV = 0x8F21

pattern GL_VERTEX_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_LENGTH_NV = 0x8F2B

pattern GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_ADDRESS_NV = 0x8F20

pattern GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_LENGTH_NV = 0x8F2A

pattern GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV  :: (Eq a, Num a) => a

pattern GL_VERTEX_ATTRIB_ARRAY_UNIFIED_NV = 0x8F1E