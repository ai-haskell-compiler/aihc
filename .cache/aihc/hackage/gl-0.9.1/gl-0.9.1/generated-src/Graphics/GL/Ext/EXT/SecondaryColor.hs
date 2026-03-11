-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.EXT.SecondaryColor (
  -- * Extension Support
    gl_EXT_secondary_color

  -- * GL_EXT_secondary_color
  , glSecondaryColor3bEXT
  , glSecondaryColor3bvEXT
  , glSecondaryColor3dEXT
  , glSecondaryColor3dvEXT
  , glSecondaryColor3fEXT
  , glSecondaryColor3fvEXT
  , glSecondaryColor3iEXT
  , glSecondaryColor3ivEXT
  , glSecondaryColor3sEXT
  , glSecondaryColor3svEXT
  , glSecondaryColor3ubEXT
  , glSecondaryColor3ubvEXT
  , glSecondaryColor3uiEXT
  , glSecondaryColor3uivEXT
  , glSecondaryColor3usEXT
  , glSecondaryColor3usvEXT
  , glSecondaryColorPointerEXT
  , pattern GL_COLOR_SUM_EXT
  , pattern GL_CURRENT_SECONDARY_COLOR_EXT
  , pattern GL_SECONDARY_COLOR_ARRAY_EXT
  , pattern GL_SECONDARY_COLOR_ARRAY_POINTER_EXT
  , pattern GL_SECONDARY_COLOR_ARRAY_SIZE_EXT
  , pattern GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT
  , pattern GL_SECONDARY_COLOR_ARRAY_TYPE_EXT
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/EXT/secondary_color.txt GL_EXT_secondary_color> extension is available.

gl_EXT_secondary_color :: Bool
gl_EXT_secondary_color = member "GL_EXT_secondary_color" extensions
{-# NOINLINE gl_EXT_secondary_color #-}

-- | Usage: @'glSecondaryColor3bEXT' red green blue@
--
-- The parameter @red@ is a @ColorB@.
--
-- The parameter @green@ is a @ColorB@.
--
-- The parameter @blue@ is a @ColorB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3b'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3bvEXT'.


glSecondaryColor3bEXT :: MonadIO m => GLbyte -> GLbyte -> GLbyte -> m ()
glSecondaryColor3bEXT = ffibytebytebyteIOV glSecondaryColor3bEXTFunPtr

glSecondaryColor3bEXTFunPtr :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
glSecondaryColor3bEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3bEXT")

{-# NOINLINE glSecondaryColor3bEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3bvEXT' v@
--
-- The parameter @v@ is a @ColorB@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3bv'.


glSecondaryColor3bvEXT :: MonadIO m => Ptr GLbyte -> m ()
glSecondaryColor3bvEXT = ffiPtrbyteIOV glSecondaryColor3bvEXTFunPtr

glSecondaryColor3bvEXTFunPtr :: FunPtr (Ptr GLbyte -> IO ())
glSecondaryColor3bvEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3bvEXT")

{-# NOINLINE glSecondaryColor3bvEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3dEXT' red green blue@
--
-- The parameter @red@ is a @ColorD@.
--
-- The parameter @green@ is a @ColorD@.
--
-- The parameter @blue@ is a @ColorD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3dvEXT'.


glSecondaryColor3dEXT :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> m ()
glSecondaryColor3dEXT = ffidoubledoubledoubleIOV glSecondaryColor3dEXTFunPtr

glSecondaryColor3dEXTFunPtr :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
glSecondaryColor3dEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3dEXT")

{-# NOINLINE glSecondaryColor3dEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3dvEXT' v@
--
-- The parameter @v@ is a @ColorD@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3dv'.


glSecondaryColor3dvEXT :: MonadIO m => Ptr GLdouble -> m ()
glSecondaryColor3dvEXT = ffiPtrdoubleIOV glSecondaryColor3dvEXTFunPtr

glSecondaryColor3dvEXTFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glSecondaryColor3dvEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3dvEXT")

{-# NOINLINE glSecondaryColor3dvEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3fEXT' red green blue@
--
-- The parameter @red@ is a @ColorF@.
--
-- The parameter @green@ is a @ColorF@.
--
-- The parameter @blue@ is a @ColorF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3fvEXT'.


glSecondaryColor3fEXT :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
glSecondaryColor3fEXT = ffifloatfloatfloatIOV glSecondaryColor3fEXTFunPtr

glSecondaryColor3fEXTFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
glSecondaryColor3fEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3fEXT")

{-# NOINLINE glSecondaryColor3fEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3fvEXT' v@
--
-- The parameter @v@ is a @ColorF@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3fv'.


glSecondaryColor3fvEXT :: MonadIO m => Ptr GLfloat -> m ()
glSecondaryColor3fvEXT = ffiPtrfloatIOV glSecondaryColor3fvEXTFunPtr

glSecondaryColor3fvEXTFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glSecondaryColor3fvEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3fvEXT")

{-# NOINLINE glSecondaryColor3fvEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3iEXT' red green blue@
--
-- The parameter @red@ is a @ColorI@.
--
-- The parameter @green@ is a @ColorI@.
--
-- The parameter @blue@ is a @ColorI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3ivEXT'.


glSecondaryColor3iEXT :: MonadIO m => GLint -> GLint -> GLint -> m ()
glSecondaryColor3iEXT = ffiintintintIOV glSecondaryColor3iEXTFunPtr

glSecondaryColor3iEXTFunPtr :: FunPtr (GLint -> GLint -> GLint -> IO ())
glSecondaryColor3iEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3iEXT")

{-# NOINLINE glSecondaryColor3iEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3ivEXT' v@
--
-- The parameter @v@ is a @ColorI@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3iv'.


glSecondaryColor3ivEXT :: MonadIO m => Ptr GLint -> m ()
glSecondaryColor3ivEXT = ffiPtrintIOV glSecondaryColor3ivEXTFunPtr

glSecondaryColor3ivEXTFunPtr :: FunPtr (Ptr GLint -> IO ())
glSecondaryColor3ivEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3ivEXT")

{-# NOINLINE glSecondaryColor3ivEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3sEXT' red green blue@
--
-- The parameter @red@ is a @ColorS@.
--
-- The parameter @green@ is a @ColorS@.
--
-- The parameter @blue@ is a @ColorS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3svEXT'.


glSecondaryColor3sEXT :: MonadIO m => GLshort -> GLshort -> GLshort -> m ()
glSecondaryColor3sEXT = ffishortshortshortIOV glSecondaryColor3sEXTFunPtr

glSecondaryColor3sEXTFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
glSecondaryColor3sEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3sEXT")

{-# NOINLINE glSecondaryColor3sEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3svEXT' v@
--
-- The parameter @v@ is a @ColorS@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3sv'.


glSecondaryColor3svEXT :: MonadIO m => Ptr GLshort -> m ()
glSecondaryColor3svEXT = ffiPtrshortIOV glSecondaryColor3svEXTFunPtr

glSecondaryColor3svEXTFunPtr :: FunPtr (Ptr GLshort -> IO ())
glSecondaryColor3svEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3svEXT")

{-# NOINLINE glSecondaryColor3svEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3ubEXT' red green blue@
--
-- The parameter @red@ is a @ColorUB@.
--
-- The parameter @green@ is a @ColorUB@.
--
-- The parameter @blue@ is a @ColorUB@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3ub'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3ubvEXT'.


glSecondaryColor3ubEXT :: MonadIO m => GLubyte -> GLubyte -> GLubyte -> m ()
glSecondaryColor3ubEXT = ffiubyteubyteubyteIOV glSecondaryColor3ubEXTFunPtr

glSecondaryColor3ubEXTFunPtr :: FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ())
glSecondaryColor3ubEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3ubEXT")

{-# NOINLINE glSecondaryColor3ubEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3ubvEXT' v@
--
-- The parameter @v@ is a @ColorUB@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3ubv'.


glSecondaryColor3ubvEXT :: MonadIO m => Ptr GLubyte -> m ()
glSecondaryColor3ubvEXT = ffiPtrubyteIOV glSecondaryColor3ubvEXTFunPtr

glSecondaryColor3ubvEXTFunPtr :: FunPtr (Ptr GLubyte -> IO ())
glSecondaryColor3ubvEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3ubvEXT")

{-# NOINLINE glSecondaryColor3ubvEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3uiEXT' red green blue@
--
-- The parameter @red@ is a @ColorUI@.
--
-- The parameter @green@ is a @ColorUI@.
--
-- The parameter @blue@ is a @ColorUI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3ui'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3uivEXT'.


glSecondaryColor3uiEXT :: MonadIO m => GLuint -> GLuint -> GLuint -> m ()
glSecondaryColor3uiEXT = ffiuintuintuintIOV glSecondaryColor3uiEXTFunPtr

glSecondaryColor3uiEXTFunPtr :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
glSecondaryColor3uiEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3uiEXT")

{-# NOINLINE glSecondaryColor3uiEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3uivEXT' v@
--
-- The parameter @v@ is a @ColorUI@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3uiv'.


glSecondaryColor3uivEXT :: MonadIO m => Ptr GLuint -> m ()
glSecondaryColor3uivEXT = ffiPtruintIOV glSecondaryColor3uivEXTFunPtr

glSecondaryColor3uivEXTFunPtr :: FunPtr (Ptr GLuint -> IO ())
glSecondaryColor3uivEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3uivEXT")

{-# NOINLINE glSecondaryColor3uivEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3usEXT' red green blue@
--
-- The parameter @red@ is a @ColorUS@.
--
-- The parameter @green@ is a @ColorUS@.
--
-- The parameter @blue@ is a @ColorUS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3us'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.EXT.SecondaryColor.glSecondaryColor3usvEXT'.


glSecondaryColor3usEXT :: MonadIO m => GLushort -> GLushort -> GLushort -> m ()
glSecondaryColor3usEXT = ffiushortushortushortIOV glSecondaryColor3usEXTFunPtr

glSecondaryColor3usEXTFunPtr :: FunPtr (GLushort -> GLushort -> GLushort -> IO ())
glSecondaryColor3usEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3usEXT")

{-# NOINLINE glSecondaryColor3usEXTFunPtr #-}

-- | Usage: @'glSecondaryColor3usvEXT' v@
--
-- The parameter @v@ is a @ColorUS@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColor3usv'.


glSecondaryColor3usvEXT :: MonadIO m => Ptr GLushort -> m ()
glSecondaryColor3usvEXT = ffiPtrushortIOV glSecondaryColor3usvEXTFunPtr

glSecondaryColor3usvEXTFunPtr :: FunPtr (Ptr GLushort -> IO ())
glSecondaryColor3usvEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColor3usvEXT")

{-# NOINLINE glSecondaryColor3usvEXTFunPtr #-}

-- | Usage: @'glSecondaryColorPointerEXT' size type stride pointer@
--
-- The parameter @type@ is a @ColorPointerType@, one of: 'Graphics.GL.Internal.Shared.GL_BYTE', 'Graphics.GL.Internal.Shared.GL_DOUBLE', 'Graphics.GL.Internal.Shared.GL_FLOAT', 'Graphics.GL.Internal.Shared.GL_INT', 'Graphics.GL.Internal.Shared.GL_SHORT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_BYTE', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_INT', 'Graphics.GL.Internal.Shared.GL_UNSIGNED_SHORT'.
--
-- The length of @pointer@ should be @COMPSIZE(size,type,stride)@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glSecondaryColorPointer'.


glSecondaryColorPointerEXT :: MonadIO m => GLint -> GLenum -> GLsizei -> Ptr () -> m ()
glSecondaryColorPointerEXT = ffiintenumsizeiPtrVIOV glSecondaryColorPointerEXTFunPtr

glSecondaryColorPointerEXTFunPtr :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
glSecondaryColorPointerEXTFunPtr = unsafePerformIO (getProcAddress "glSecondaryColorPointerEXT")

{-# NOINLINE glSecondaryColorPointerEXTFunPtr #-}

pattern GL_COLOR_SUM_EXT  :: (Eq a, Num a) => a

pattern GL_COLOR_SUM_EXT = 0x8458

pattern GL_CURRENT_SECONDARY_COLOR_EXT  :: (Eq a, Num a) => a

pattern GL_CURRENT_SECONDARY_COLOR_EXT = 0x8459

pattern GL_SECONDARY_COLOR_ARRAY_EXT  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_EXT = 0x845E

pattern GL_SECONDARY_COLOR_ARRAY_POINTER_EXT  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_POINTER_EXT = 0x845D

pattern GL_SECONDARY_COLOR_ARRAY_SIZE_EXT  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_SIZE_EXT = 0x845A

pattern GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT = 0x845C

pattern GL_SECONDARY_COLOR_ARRAY_TYPE_EXT  :: (Eq a, Num a) => a

pattern GL_SECONDARY_COLOR_ARRAY_TYPE_EXT = 0x845B