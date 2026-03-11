-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.CoordinateFrame (
  -- * Extension Support
    gl_EXT_coordinate_frame

  -- * GL_EXT_coordinate_frame
  , glBinormal3bEXT
  , glBinormal3bvEXT
  , glBinormal3dEXT
  , glBinormal3dvEXT
  , glBinormal3fEXT
  , glBinormal3fvEXT
  , glBinormal3iEXT
  , glBinormal3ivEXT
  , glBinormal3sEXT
  , glBinormal3svEXT
  , glBinormalPointerEXT
  , glTangent3bEXT
  , glTangent3bvEXT
  , glTangent3dEXT
  , glTangent3dvEXT
  , glTangent3fEXT
  , glTangent3fvEXT
  , glTangent3iEXT
  , glTangent3ivEXT
  , glTangent3sEXT
  , glTangent3svEXT
  , glTangentPointerEXT
  , pattern GL_BINORMAL_ARRAY_EXT
  , pattern GL_BINORMAL_ARRAY_POINTER_EXT
  , pattern GL_BINORMAL_ARRAY_STRIDE_EXT
  , pattern GL_BINORMAL_ARRAY_TYPE_EXT
  , pattern GL_CURRENT_BINORMAL_EXT
  , pattern GL_CURRENT_TANGENT_EXT
  , pattern GL_MAP1_BINORMAL_EXT
  , pattern GL_MAP1_TANGENT_EXT
  , pattern GL_MAP2_BINORMAL_EXT
  , pattern GL_MAP2_TANGENT_EXT
  , pattern GL_TANGENT_ARRAY_EXT
  , pattern GL_TANGENT_ARRAY_POINTER_EXT
  , pattern GL_TANGENT_ARRAY_STRIDE_EXT
  , pattern GL_TANGENT_ARRAY_TYPE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/coordinate_frame.txt GL_EXT_coordinate_frame> extension is available.

gl_EXT_coordinate_frame :: Bool
gl_EXT_coordinate_frame = member "GL_EXT_coordinate_frame" extensions
{-# NOINLINE gl_EXT_coordinate_frame #-}

-- | Usage: @'glBinormal3bEXT' bx by bz@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glBinormal3bvEXT'.


glBinormal3bEXT :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> m ()
glBinormal3bEXT = ffibytebytebyteIOV glBinormal3bEXTFunPtr

glBinormal3bEXTFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
glBinormal3bEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3bEXT")

{-# NOINLINE glBinormal3bEXTFunPtr #-}

-- | Usage: @'glBinormal3bvEXT' v@
--
-- The length of @v@ should be @3@.


glBinormal3bvEXT :: MonadIO m => Ptr GLbyte -> m ()
glBinormal3bvEXT = ffiPtrbyteIOV glBinormal3bvEXTFunPtr

glBinormal3bvEXTFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glBinormal3bvEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3bvEXT")

{-# NOINLINE glBinormal3bvEXTFunPtr #-}

-- | Usage: @'glBinormal3dEXT' bx by bz@
--
-- The parameter @bx@ is a @CoordD@.
--
-- The parameter @by@ is a @CoordD@.
--
-- The parameter @bz@ is a @CoordD@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glBinormal3dvEXT'.


glBinormal3dEXT :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> m ()
glBinormal3dEXT = ffidoubledoubledoubleIOV glBinormal3dEXTFunPtr

glBinormal3dEXTFunPtr :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
glBinormal3dEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3dEXT")

{-# NOINLINE glBinormal3dEXTFunPtr #-}

-- | Usage: @'glBinormal3dvEXT' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @3@.


glBinormal3dvEXT :: MonadIO m => Ptr GLdouble -> m ()
glBinormal3dvEXT = ffiPtrdoubleIOV glBinormal3dvEXTFunPtr

glBinormal3dvEXTFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glBinormal3dvEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3dvEXT")

{-# NOINLINE glBinormal3dvEXTFunPtr #-}

-- | Usage: @'glBinormal3fEXT' bx by bz@
--
-- The parameter @bx@ is a @CoordF@.
--
-- The parameter @by@ is a @CoordF@.
--
-- The parameter @bz@ is a @CoordF@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glBinormal3fvEXT'.


glBinormal3fEXT :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
glBinormal3fEXT = ffifloatfloatfloatIOV glBinormal3fEXTFunPtr

glBinormal3fEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
glBinormal3fEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3fEXT")

{-# NOINLINE glBinormal3fEXTFunPtr #-}

-- | Usage: @'glBinormal3fvEXT' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @3@.


glBinormal3fvEXT :: MonadIO m => Ptr GLfloat -> m ()
glBinormal3fvEXT = ffiPtrfloatIOV glBinormal3fvEXTFunPtr

glBinormal3fvEXTFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glBinormal3fvEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3fvEXT")

{-# NOINLINE glBinormal3fvEXTFunPtr #-}

-- | Usage: @'glBinormal3iEXT' bx by bz@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glBinormal3ivEXT'.


glBinormal3iEXT :: MonadIO m => GLint -> GLint -> GLint -> m ()
glBinormal3iEXT = ffiintintintIOV glBinormal3iEXTFunPtr

glBinormal3iEXTFunPtr :: FunPtr (GLint -> GLint -> GLint -> IO ())
glBinormal3iEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3iEXT")

{-# NOINLINE glBinormal3iEXTFunPtr #-}

-- | Usage: @'glBinormal3ivEXT' v@
--
-- The length of @v@ should be @3@.


glBinormal3ivEXT :: MonadIO m => Ptr GLint -> m ()
glBinormal3ivEXT = ffiPtrintIOV glBinormal3ivEXTFunPtr

glBinormal3ivEXTFunPtr :: FunPtr (Ptr GLint -> IO ())
glBinormal3ivEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3ivEXT")

{-# NOINLINE glBinormal3ivEXTFunPtr #-}

-- | Usage: @'glBinormal3sEXT' bx by bz@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glBinormal3svEXT'.


glBinormal3sEXT :: MonadIO m => GLshort -> GLshort -> GLshort -> m ()
glBinormal3sEXT = ffishortshortshortIOV glBinormal3sEXTFunPtr

glBinormal3sEXTFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
glBinormal3sEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3sEXT")

{-# NOINLINE glBinormal3sEXTFunPtr #-}

-- | Usage: @'glBinormal3svEXT' v@
--
-- The length of @v@ should be @3@.


glBinormal3svEXT :: MonadIO m => Ptr GLshort -> m ()
glBinormal3svEXT = ffiPtrshortIOV glBinormal3svEXTFunPtr

glBinormal3svEXTFunPtr :: FunPtr (Ptr GLshort -> IO ())
glBinormal3svEXTFunPtr = unsafePerformIO (getProcAddress "glBinormal3svEXT")

{-# NOINLINE glBinormal3svEXTFunPtr #-}

-- | Usage: @'glBinormalPointerEXT' type stride pointer@
--
-- The parameter @type@ is a @BinormalPointerTypeEXT@.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glBinormalPointerEXT :: MonadIO m => GLenum -> GLsizei -> Ptr () -> m ()
glBinormalPointerEXT = ffienumsizeiPtrVIOV glBinormalPointerEXTFunPtr

glBinormalPointerEXTFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ())
glBinormalPointerEXTFunPtr = unsafePerformIO (getProcAddress "glBinormalPointerEXT")

{-# NOINLINE glBinormalPointerEXTFunPtr #-}

-- | Usage: @'glTangent3bEXT' tx ty tz@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glTangent3bvEXT'.


glTangent3bEXT :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> m ()
glTangent3bEXT = ffibytebytebyteIOV glTangent3bEXTFunPtr

glTangent3bEXTFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
glTangent3bEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3bEXT")

{-# NOINLINE glTangent3bEXTFunPtr #-}

-- | Usage: @'glTangent3bvEXT' v@
--
-- The length of @v@ should be @3@.


glTangent3bvEXT :: MonadIO m => Ptr GLbyte -> m ()
glTangent3bvEXT = ffiPtrbyteIOV glTangent3bvEXTFunPtr

glTangent3bvEXTFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glTangent3bvEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3bvEXT")

{-# NOINLINE glTangent3bvEXTFunPtr #-}

-- | Usage: @'glTangent3dEXT' tx ty tz@
--
-- The parameter @tx@ is a @CoordD@.
--
-- The parameter @ty@ is a @CoordD@.
--
-- The parameter @tz@ is a @CoordD@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glTangent3dvEXT'.


glTangent3dEXT :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> m ()
glTangent3dEXT = ffidoubledoubledoubleIOV glTangent3dEXTFunPtr

glTangent3dEXTFunPtr :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
glTangent3dEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3dEXT")

{-# NOINLINE glTangent3dEXTFunPtr #-}

-- | Usage: @'glTangent3dvEXT' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @3@.


glTangent3dvEXT :: MonadIO m => Ptr GLdouble -> m ()
glTangent3dvEXT = ffiPtrdoubleIOV glTangent3dvEXTFunPtr

glTangent3dvEXTFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glTangent3dvEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3dvEXT")

{-# NOINLINE glTangent3dvEXTFunPtr #-}

-- | Usage: @'glTangent3fEXT' tx ty tz@
--
-- The parameter @tx@ is a @CoordF@.
--
-- The parameter @ty@ is a @CoordF@.
--
-- The parameter @tz@ is a @CoordF@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glTangent3fvEXT'.


glTangent3fEXT :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
glTangent3fEXT = ffifloatfloatfloatIOV glTangent3fEXTFunPtr

glTangent3fEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
glTangent3fEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3fEXT")

{-# NOINLINE glTangent3fEXTFunPtr #-}

-- | Usage: @'glTangent3fvEXT' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @3@.


glTangent3fvEXT :: MonadIO m => Ptr GLfloat -> m ()
glTangent3fvEXT = ffiPtrfloatIOV glTangent3fvEXTFunPtr

glTangent3fvEXTFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glTangent3fvEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3fvEXT")

{-# NOINLINE glTangent3fvEXTFunPtr #-}

-- | Usage: @'glTangent3iEXT' tx ty tz@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glTangent3ivEXT'.


glTangent3iEXT :: MonadIO m => GLint -> GLint -> GLint -> m ()
glTangent3iEXT = ffiintintintIOV glTangent3iEXTFunPtr

glTangent3iEXTFunPtr :: FunPtr (GLint -> GLint -> GLint -> IO ())
glTangent3iEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3iEXT")

{-# NOINLINE glTangent3iEXTFunPtr #-}

-- | Usage: @'glTangent3ivEXT' v@
--
-- The length of @v@ should be @3@.


glTangent3ivEXT :: MonadIO m => Ptr GLint -> m ()
glTangent3ivEXT = ffiPtrintIOV glTangent3ivEXTFunPtr

glTangent3ivEXTFunPtr :: FunPtr (Ptr GLint -> IO ())
glTangent3ivEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3ivEXT")

{-# NOINLINE glTangent3ivEXTFunPtr #-}

-- | Usage: @'glTangent3sEXT' tx ty tz@
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.CoordinateFrame.glTangent3svEXT'.


glTangent3sEXT :: MonadIO m => GLshort -> GLshort -> GLshort -> m ()
glTangent3sEXT = ffishortshortshortIOV glTangent3sEXTFunPtr

glTangent3sEXTFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
glTangent3sEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3sEXT")

{-# NOINLINE glTangent3sEXTFunPtr #-}

-- | Usage: @'glTangent3svEXT' v@
--
-- The length of @v@ should be @3@.


glTangent3svEXT :: MonadIO m => Ptr GLshort -> m ()
glTangent3svEXT = ffiPtrshortIOV glTangent3svEXTFunPtr

glTangent3svEXTFunPtr :: FunPtr (Ptr GLshort -> IO ())
glTangent3svEXTFunPtr = unsafePerformIO (getProcAddress "glTangent3svEXT")

{-# NOINLINE glTangent3svEXTFunPtr #-}

-- | Usage: @'glTangentPointerEXT' type stride pointer@
--
-- The parameter @type@ is a @TangentPointerTypeEXT@.
--
-- The length of @pointer@ should be @COMPSIZE(type,stride)@.


glTangentPointerEXT :: MonadIO m => GLenum -> GLsizei -> Ptr () -> m ()
glTangentPointerEXT = ffienumsizeiPtrVIOV glTangentPointerEXTFunPtr

glTangentPointerEXTFunPtr :: FunPtr (GLenum -> GLsizei -> Ptr () -> IO ())
glTangentPointerEXTFunPtr = unsafePerformIO (getProcAddress "glTangentPointerEXT")

{-# NOINLINE glTangentPointerEXTFunPtr #-}

pattern GL_BINORMAL_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_BINORMAL_ARRAY_EXT = 0x843A

pattern GL_BINORMAL_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_BINORMAL_ARRAY_POINTER_EXT = 0x8443

pattern GL_BINORMAL_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_BINORMAL_ARRAY_STRIDE_EXT = 0x8441

pattern GL_BINORMAL_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_BINORMAL_ARRAY_TYPE_EXT = 0x8440

pattern GL_CURRENT_BINORMAL_EXT  :: (Eq a, Num a) => a

pattern GL_CURRENT_BINORMAL_EXT = 0x843C

pattern GL_CURRENT_TANGENT_EXT  :: (Eq a, Num a) => a

pattern GL_CURRENT_TANGENT_EXT = 0x843B

pattern GL_MAP1_BINORMAL_EXT  :: (Eq a, Num a) => a

pattern GL_MAP1_BINORMAL_EXT = 0x8446

pattern GL_MAP1_TANGENT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP1_TANGENT_EXT = 0x8444

pattern GL_MAP2_BINORMAL_EXT  :: (Eq a, Num a) => a

pattern GL_MAP2_BINORMAL_EXT = 0x8447

pattern GL_MAP2_TANGENT_EXT  :: (Eq a, Num a) => a

pattern GL_MAP2_TANGENT_EXT = 0x8445

pattern GL_TANGENT_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_TANGENT_ARRAY_EXT = 0x8439

pattern GL_TANGENT_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_TANGENT_ARRAY_POINTER_EXT = 0x8442

pattern GL_TANGENT_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_TANGENT_ARRAY_STRIDE_EXT = 0x843F

pattern GL_TANGENT_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_TANGENT_ARRAY_TYPE_EXT = 0x843E