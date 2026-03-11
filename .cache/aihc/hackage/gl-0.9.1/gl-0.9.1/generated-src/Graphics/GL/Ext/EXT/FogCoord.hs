-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.FogCoord (
  -- * Extension Support
    gl_EXT_fog_coord

  -- * GL_EXT_fog_coord
  , glFogCoordPointerEXT
  , glFogCoorddEXT
  , glFogCoorddvEXT
  , glFogCoordfEXT
  , glFogCoordfvEXT
  , pattern GL_CURRENT_FOG_COORDINATE_EXT
  , pattern GL_FOG_COORDINATE_ARRAY_EXT
  , pattern GL_FOG_COORDINATE_ARRAY_POINTER_EXT
  , pattern GL_FOG_COORDINATE_ARRAY_STRIDE_EXT
  , pattern GL_FOG_COORDINATE_ARRAY_TYPE_EXT
  , pattern GL_FOG_COORDINATE_EXT
  , pattern GL_FOG_COORDINATE_SOURCE_EXT
  , pattern GL_FRAGMENT_DEPTH_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Internal.Shared
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/fog_coord.txt GL_EXT_fog_coord> extension is available.

gl_EXT_fog_coord :: Bool
gl_EXT_fog_coord = member "GL_EXT_fog_coord" extensions
{-# NOINLINE gl_EXT_fog_coord #-}

-- | Usage: @'glFogCoordPointerEXT' type stride pointer@
--
-- The parameter @type@ is a @FogPointerTypeEXT@, one of: 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_DOUBLE'.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFogCoordPointer'.


glFogCoordPointerEXT :: MonadIO m => GLenum -> GLsizei -> Ptr () -> m ()
glFogCoordPointerEXT = ffienumsizeiPtrVIOV glFogCoordPointerEXTFunPtr

glFogCoordPointerEXTFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ())
glFogCoordPointerEXTFunPtr = unsafePerformIO (getProcAddress "glFogCoordPointerEXT")

{-# NOINLINE glFogCoordPointerEXTFunPtr #-}

-- | Usage: @'glFogCoorddEXT' coord@
--
-- The parameter @coord@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFogCoordd'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.FogCoord.glFogCoorddvEXT'.


glFogCoorddEXT :: MonadIO m => GLdouble -> m ()
glFogCoorddEXT = ffidoubleIOV glFogCoorddEXTFunPtr

glFogCoorddEXTFunPtr :: FunPtr (GLdouble -> IO ())
glFogCoorddEXTFunPtr = unsafePerformIO (getProcAddress "glFogCoorddEXT")

{-# NOINLINE glFogCoorddEXTFunPtr #-}

-- | Usage: @'glFogCoorddvEXT' coord@
--
-- The parameter @coord@ is a @CoordD@.
--
-- The length of @coord@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFogCoorddv'.


glFogCoorddvEXT :: MonadIO m => Ptr GLdouble -> m ()
glFogCoorddvEXT = ffiPtrdoubleIOV glFogCoorddvEXTFunPtr

glFogCoorddvEXTFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glFogCoorddvEXTFunPtr = unsafePerformIO (getProcAddress "glFogCoorddvEXT")

{-# NOINLINE glFogCoorddvEXTFunPtr #-}

-- | Usage: @'glFogCoordfEXT' coord@
--
-- The parameter @coord@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFogCoordf'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.FogCoord.glFogCoordfvEXT'.


glFogCoordfEXT :: MonadIO m => GLfloat -> m ()
glFogCoordfEXT = ffifloatIOV glFogCoordfEXTFunPtr

glFogCoordfEXTFunPtr :: FunPtr (GLfloat -> IO ())
glFogCoordfEXTFunPtr = unsafePerformIO (getProcAddress "glFogCoordfEXT")

{-# NOINLINE glFogCoordfEXTFunPtr #-}

-- | Usage: @'glFogCoordfvEXT' coord@
--
-- The parameter @coord@ is a @CoordF@.
--
-- The length of @coord@ should be @1@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glFogCoordfv'.


glFogCoordfvEXT :: MonadIO m => Ptr GLfloat -> m ()
glFogCoordfvEXT = ffiPtrfloatIOV glFogCoordfvEXTFunPtr

glFogCoordfvEXTFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glFogCoordfvEXTFunPtr = unsafePerformIO (getProcAddress "glFogCoordfvEXT")

{-# NOINLINE glFogCoordfvEXTFunPtr #-}

pattern GL_CURRENT_FOG_COORDINATE_EXT  :: (Eq a, Num a) => a

pattern GL_CURRENT_FOG_COORDINATE_EXT = 0x8453

pattern GL_FOG_COORDINATE_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_EXT = 0x8457

pattern GL_FOG_COORDINATE_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_POINTER_EXT = 0x8456

pattern GL_FOG_COORDINATE_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_STRIDE_EXT = 0x8455

pattern GL_FOG_COORDINATE_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_ARRAY_TYPE_EXT = 0x8454

pattern GL_FOG_COORDINATE_EXT  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_EXT = 0x8451

pattern GL_FOG_COORDINATE_SOURCE_EXT  :: (Eq a, Num a) => a

pattern GL_FOG_COORDINATE_SOURCE_EXT = 0x8450