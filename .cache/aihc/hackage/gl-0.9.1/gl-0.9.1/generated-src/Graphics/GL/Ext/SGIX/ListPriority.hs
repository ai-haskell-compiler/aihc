-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SGIX.ListPriority (
  -- * Extension Support
    gl_SGIX_list_priority

  -- * GL_SGIX_list_priority
  , glGetListParameterfvSGIX
  , glGetListParameterivSGIX
  , glListParameterfSGIX
  , glListParameterfvSGIX
  , glListParameteriSGIX
  , glListParameterivSGIX
  , pattern GL_LIST_PRIORITY_SGIX
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SGIX/list_priority.txt GL_SGIX_list_priority> extension is available.

gl_SGIX_list_priority :: Bool
gl_SGIX_list_priority = member "GL_SGIX_list_priority" extensions
{-# NOINLINE gl_SGIX_list_priority #-}

-- | Usage: @'glGetListParameterfvSGIX' list pname params@
--
-- The parameter @list@ is a @List@.
--
-- The parameter @pname@ is a @ListParameterName@, one of: 'Graphics.GL.Ext.SGIX.ListPriority.GL_LIST_PRIORITY_SGIX'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetListParameterfvSGIX :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glGetListParameterfvSGIX = ffiuintenumPtrfloatIOV glGetListParameterfvSGIXFunPtr

glGetListParameterfvSGIXFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glGetListParameterfvSGIXFunPtr = unsafePerformIO (getProcAddress "glGetListParameterfvSGIX")

{-# NOINLINE glGetListParameterfvSGIXFunPtr #-}

-- | Usage: @'glGetListParameterivSGIX' list pname params@
--
-- The parameter @list@ is a @List@.
--
-- The parameter @pname@ is a @ListParameterName@, one of: 'Graphics.GL.Ext.SGIX.ListPriority.GL_LIST_PRIORITY_SGIX'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glGetListParameterivSGIX :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glGetListParameterivSGIX = ffiuintenumPtrintIOV glGetListParameterivSGIXFunPtr

glGetListParameterivSGIXFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glGetListParameterivSGIXFunPtr = unsafePerformIO (getProcAddress "glGetListParameterivSGIX")

{-# NOINLINE glGetListParameterivSGIXFunPtr #-}

-- | Usage: @'glListParameterfSGIX' list pname param@
--
-- The parameter @list@ is a @List@.
--
-- The parameter @pname@ is a @ListParameterName@, one of: 'Graphics.GL.Ext.SGIX.ListPriority.GL_LIST_PRIORITY_SGIX'.
--
-- The parameter @param@ is a @CheckedFloat32@.


glListParameterfSGIX :: MonadIO m => GLuint -> GLenum -> GLfloat -> m ()
glListParameterfSGIX = ffiuintenumfloatIOV glListParameterfSGIXFunPtr

glListParameterfSGIXFunPtr :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
glListParameterfSGIXFunPtr = unsafePerformIO (getProcAddress "glListParameterfSGIX")

{-# NOINLINE glListParameterfSGIXFunPtr #-}

-- | Usage: @'glListParameterfvSGIX' list pname params@
--
-- The parameter @list@ is a @List@.
--
-- The parameter @pname@ is a @ListParameterName@, one of: 'Graphics.GL.Ext.SGIX.ListPriority.GL_LIST_PRIORITY_SGIX'.
--
-- The parameter @params@ is a @CheckedFloat32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glListParameterfvSGIX :: MonadIO m => GLuint -> GLenum -> Ptr GLfloat -> m ()
glListParameterfvSGIX = ffiuintenumPtrfloatIOV glListParameterfvSGIXFunPtr

glListParameterfvSGIXFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
glListParameterfvSGIXFunPtr = unsafePerformIO (getProcAddress "glListParameterfvSGIX")

{-# NOINLINE glListParameterfvSGIXFunPtr #-}

-- | Usage: @'glListParameteriSGIX' list pname param@
--
-- The parameter @list@ is a @List@.
--
-- The parameter @pname@ is a @ListParameterName@, one of: 'Graphics.GL.Ext.SGIX.ListPriority.GL_LIST_PRIORITY_SGIX'.
--
-- The parameter @param@ is a @CheckedInt32@.


glListParameteriSGIX :: MonadIO m => GLuint -> GLenum -> GLint -> m ()
glListParameteriSGIX = ffiuintenumintIOV glListParameteriSGIXFunPtr

glListParameteriSGIXFunPtr :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
glListParameteriSGIXFunPtr = unsafePerformIO (getProcAddress "glListParameteriSGIX")

{-# NOINLINE glListParameteriSGIXFunPtr #-}

-- | Usage: @'glListParameterivSGIX' list pname params@
--
-- The parameter @list@ is a @List@.
--
-- The parameter @pname@ is a @ListParameterName@, one of: 'Graphics.GL.Ext.SGIX.ListPriority.GL_LIST_PRIORITY_SGIX'.
--
-- The parameter @params@ is a @CheckedInt32@.
--
-- The length of @params@ should be @COMPSIZE(pname)@.


glListParameterivSGIX :: MonadIO m => GLuint -> GLenum -> Ptr GLint -> m ()
glListParameterivSGIX = ffiuintenumPtrintIOV glListParameterivSGIXFunPtr

glListParameterivSGIXFunPtr :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
glListParameterivSGIXFunPtr = unsafePerformIO (getProcAddress "glListParameterivSGIX")

{-# NOINLINE glListParameterivSGIXFunPtr #-}

pattern GL_LIST_PRIORITY_SGIX  :: (Eq a, Num a) => a

pattern GL_LIST_PRIORITY_SGIX = 0x8182