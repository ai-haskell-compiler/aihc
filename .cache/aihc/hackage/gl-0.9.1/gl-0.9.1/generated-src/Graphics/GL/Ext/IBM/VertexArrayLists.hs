-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IBM.VertexArrayLists (
  -- * Extension Support
    gl_IBM_vertex_array_lists

  -- * GL_IBM_vertex_array_lists
  , glColorPointerListIBM
  , glEdgeFlagPointerListIBM
  , glFogCoordPointerListIBM
  , glIndexPointerListIBM
  , glNormalPointerListIBM
  , glSecondaryColorPointerListIBM
  , glTexCoordPointerListIBM
  , glVertexPointerListIBM
  , pattern GL_COLOR_ARRAY_LIST_IBM
  , pattern GL_COLOR_ARRAY_LIST_STRIDE_IBM
  , pattern GL_EDGE_FLAG_ARRAY_LIST_IBM
  , pattern GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM
  , pattern GL_FOG_COORDINATE_ARRAY_LIST_IBM
  , pattern GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM
  , pattern GL_INDEX_ARRAY_LIST_IBM
  , pattern GL_INDEX_ARRAY_LIST_STRIDE_IBM
  , pattern GL_NORMAL_ARRAY_LIST_IBM
  , pattern GL_NORMAL_ARRAY_LIST_STRIDE_IBM
  , pattern GL_SECONDARY_COLOR_ARRAY_LIST_IBM
  , pattern GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM
  , pattern GL_TEXTURE_COORD_ARRAY_LIST_IBM
  , pattern GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM
  , pattern GL_VERTEX_ARRAY_LIST_IBM
  , pattern GL_VERTEX_ARRAY_LIST_STRIDE_IBM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/IBM/vertex_array_lists.txt GL_IBM_vertex_array_lists> extension is available.

gl_IBM_vertex_array_lists :: Bool
gl_IBM_vertex_array_lists = member "GL_IBM_vertex_array_lists" extensions
{-# NOINLINE gl_IBM_vertex_array_lists #-}

-- | Usage: @'glColorPointerListIBM' size type stride pointer ptrstride@
--
-- The parameter @type@ is a @ColorPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glColorPointerListIBM :: MonadIO m => GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glColorPointerListIBM = ffiintenumintPtrPtrVintIOV glColorPointerListIBMFunPtr

glColorPointerListIBMFunPtr :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glColorPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glColorPointerListIBM")

{-# NOINLINE glColorPointerListIBMFunPtr #-}

-- | Usage: @'glEdgeFlagPointerListIBM' stride pointer ptrstride@
--
-- The parameter @pointer@ is a @BooleanPointer@.
--
-- The length of @pointer@ should be @COMPSIZE(stride)@.


glEdgeFlagPointerListIBM :: MonadIO m => GLint -> Ptr (Ptr GLboolean) -> GLint -> m ()
glEdgeFlagPointerListIBM = ffiintPtrPtrbooleanintIOV glEdgeFlagPointerListIBMFunPtr

glEdgeFlagPointerListIBMFunPtr :: FunPtr (GLint -> Ptr (Ptr GLboolean) -> GLint -> IO ())
glEdgeFlagPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glEdgeFlagPointerListIBM")

{-# NOINLINE glEdgeFlagPointerListIBMFunPtr #-}

-- | Usage: @'glFogCoordPointerListIBM' type stride pointer ptrstride@
--
-- The parameter @type@ is a @FogPointerTypeIBM@, one of: 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_DOUBLE'.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glFogCoordPointerListIBM :: MonadIO m => GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glFogCoordPointerListIBM = ffienumintPtrPtrVintIOV glFogCoordPointerListIBMFunPtr

glFogCoordPointerListIBMFunPtr :: FunPtr (GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glFogCoordPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glFogCoordPointerListIBM")

{-# NOINLINE glFogCoordPointerListIBMFunPtr #-}

-- | Usage: @'glIndexPointerListIBM' type stride pointer ptrstride@
--
-- The parameter @type@ is a @IndexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glIndexPointerListIBM :: MonadIO m => GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glIndexPointerListIBM = ffienumintPtrPtrVintIOV glIndexPointerListIBMFunPtr

glIndexPointerListIBMFunPtr :: FunPtr (GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glIndexPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glIndexPointerListIBM")

{-# NOINLINE glIndexPointerListIBMFunPtr #-}

-- | Usage: @'glNormalPointerListIBM' type stride pointer ptrstride@
--
-- The parameter @type@ is a @NormalPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glNormalPointerListIBM :: MonadIO m => GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glNormalPointerListIBM = ffienumintPtrPtrVintIOV glNormalPointerListIBMFunPtr

glNormalPointerListIBMFunPtr :: FunPtr (GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glNormalPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glNormalPointerListIBM")

{-# NOINLINE glNormalPointerListIBMFunPtr #-}

-- | Usage: @'glSecondaryColorPointerListIBM' size type stride pointer ptrstride@
--
-- The parameter @type@ is a @SecondaryColorPointerTypeIBM@.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glSecondaryColorPointerListIBM :: MonadIO m => GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glSecondaryColorPointerListIBM = ffiintenumintPtrPtrVintIOV glSecondaryColorPointerListIBMFunPtr

glSecondaryColorPointerListIBMFunPtr :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glSecondaryColorPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glSecondaryColorPointerListIBM")

{-# NOINLINE glSecondaryColorPointerListIBMFunPtr #-}

-- | Usage: @'glTexCoordPointerListIBM' size type stride pointer ptrstride@
--
-- The parameter @type@ is a @TexCoordPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glTexCoordPointerListIBM :: MonadIO m => GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glTexCoordPointerListIBM = ffiintenumintPtrPtrVintIOV glTexCoordPointerListIBMFunPtr

glTexCoordPointerListIBMFunPtr :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glTexCoordPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glTexCoordPointerListIBM")

{-# NOINLINE glTexCoordPointerListIBMFunPtr #-}

-- | Usage: @'glVertexPointerListIBM' size type stride pointer ptrstride@
--
-- The parameter @type@ is a @VertexPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.


glVertexPointerListIBM :: MonadIO m => GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> m ()
glVertexPointerListIBM = ffiintenumintPtrPtrVintIOV glVertexPointerListIBMFunPtr

glVertexPointerListIBMFunPtr :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr ()) -> GLint -> IO ())
glVertexPointerListIBMFunPtr = unsafePerformIO (getProcAddress "glVertexPointerListIBM")

{-# NOINLINE glVertexPointerListIBMFunPtr #-}

pattern GL_COLOR_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_LIST_IBM = 103072

pattern GL_COLOR_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_COLOR_ARRAY_LIST_STRIDE_IBM = 103082

pattern GL_EDGE_FLAG_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_LIST_IBM = 103075

pattern GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM = 103085

pattern GL_FOG_COORDINATE_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_LIST_IBM = 103076

pattern GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM = 103086

pattern GL_INDEX_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_LIST_IBM = 103073

pattern GL_INDEX_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_INDEX_ARRAY_LIST_STRIDE_IBM = 103083

pattern GL_NORMAL_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_LIST_IBM = 103071

pattern GL_NORMAL_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_NORMAL_ARRAY_LIST_STRIDE_IBM = 103081

pattern GL_SECONDARY_COLOR_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_LIST_IBM = 103077

pattern GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM = 103087

pattern GL_TEXTURE_COORD_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_LIST_IBM = 103074

pattern GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM = 103084

pattern GL_VERTEX_ARRAY_LIST_IBM  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_LIST_IBM = 103070

pattern GL_VERTEX_ARRAY_LIST_STRIDE_IBM  :: (Eq a, Num a) => a

pattern GL_VERTEX_ARRAY_LIST_STRIDE_IBM = 103080