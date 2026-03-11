-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ARB.WindowPos (
  -- * Extension Support
    gl_ARB_window_pos

  -- * GL_ARB_window_pos
  , glWindowPos2dARB
  , glWindowPos2dvARB
  , glWindowPos2fARB
  , glWindowPos2fvARB
  , glWindowPos2iARB
  , glWindowPos2ivARB
  , glWindowPos2sARB
  , glWindowPos2svARB
  , glWindowPos3dARB
  , glWindowPos3dvARB
  , glWindowPos3fARB
  , glWindowPos3fvARB
  , glWindowPos3iARB
  , glWindowPos3ivARB
  , glWindowPos3sARB
  , glWindowPos3svARB
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/ARB/window_pos.txt GL_ARB_window_pos> extension is available.

gl_ARB_window_pos :: Bool
gl_ARB_window_pos = member "GL_ARB_window_pos" extensions
{-# NOINLINE gl_ARB_window_pos #-}

-- | Usage: @'glWindowPos2dARB' x y@
--
-- The parameter @x@ is a @CoordD@.
--
-- The parameter @y@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos2dvARB'.


glWindowPos2dARB :: MonadIO m => GLdouble -> GLdouble -> m ()
glWindowPos2dARB = ffidoubledoubleIOV glWindowPos2dARBFunPtr

glWindowPos2dARBFunPtr :: FunPtr (GLdouble -> GLdouble -> IO ())
glWindowPos2dARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2dARB")

{-# NOINLINE glWindowPos2dARBFunPtr #-}

-- | Usage: @'glWindowPos2dvARB' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2dv'.


glWindowPos2dvARB :: MonadIO m => Ptr GLdouble -> m ()
glWindowPos2dvARB = ffiPtrdoubleIOV glWindowPos2dvARBFunPtr

glWindowPos2dvARBFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glWindowPos2dvARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2dvARB")

{-# NOINLINE glWindowPos2dvARBFunPtr #-}

-- | Usage: @'glWindowPos2fARB' x y@
--
-- The parameter @x@ is a @CoordF@.
--
-- The parameter @y@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos2fvARB'.


glWindowPos2fARB :: MonadIO m => GLfloat -> GLfloat -> m ()
glWindowPos2fARB = ffifloatfloatIOV glWindowPos2fARBFunPtr

glWindowPos2fARBFunPtr :: FunPtr (GLfloat -> GLfloat -> IO ())
glWindowPos2fARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2fARB")

{-# NOINLINE glWindowPos2fARBFunPtr #-}

-- | Usage: @'glWindowPos2fvARB' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2fv'.


glWindowPos2fvARB :: MonadIO m => Ptr GLfloat -> m ()
glWindowPos2fvARB = ffiPtrfloatIOV glWindowPos2fvARBFunPtr

glWindowPos2fvARBFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glWindowPos2fvARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2fvARB")

{-# NOINLINE glWindowPos2fvARBFunPtr #-}

-- | Usage: @'glWindowPos2iARB' x y@
--
-- The parameter @x@ is a @CoordI@.
--
-- The parameter @y@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos2ivARB'.


glWindowPos2iARB :: MonadIO m => GLint -> GLint -> m ()
glWindowPos2iARB = ffiintintIOV glWindowPos2iARBFunPtr

glWindowPos2iARBFunPtr :: FunPtr (GLint -> GLint -> IO ())
glWindowPos2iARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2iARB")

{-# NOINLINE glWindowPos2iARBFunPtr #-}

-- | Usage: @'glWindowPos2ivARB' v@
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2iv'.


glWindowPos2ivARB :: MonadIO m => Ptr GLint -> m ()
glWindowPos2ivARB = ffiPtrintIOV glWindowPos2ivARBFunPtr

glWindowPos2ivARBFunPtr :: FunPtr (Ptr GLint -> IO ())
glWindowPos2ivARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2ivARB")

{-# NOINLINE glWindowPos2ivARBFunPtr #-}

-- | Usage: @'glWindowPos2sARB' x y@
--
-- The parameter @x@ is a @CoordS@.
--
-- The parameter @y@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos2svARB'.


glWindowPos2sARB :: MonadIO m => GLshort -> GLshort -> m ()
glWindowPos2sARB = ffishortshortIOV glWindowPos2sARBFunPtr

glWindowPos2sARBFunPtr :: FunPtr (GLshort -> GLshort -> IO ())
glWindowPos2sARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2sARB")

{-# NOINLINE glWindowPos2sARBFunPtr #-}

-- | Usage: @'glWindowPos2svARB' v@
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2sv'.


glWindowPos2svARB :: MonadIO m => Ptr GLshort -> m ()
glWindowPos2svARB = ffiPtrshortIOV glWindowPos2svARBFunPtr

glWindowPos2svARBFunPtr :: FunPtr (Ptr GLshort -> IO ())
glWindowPos2svARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos2svARB")

{-# NOINLINE glWindowPos2svARBFunPtr #-}

-- | Usage: @'glWindowPos3dARB' x y z@
--
-- The parameter @x@ is a @CoordD@.
--
-- The parameter @y@ is a @CoordD@.
--
-- The parameter @z@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos3dvARB'.


glWindowPos3dARB :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> m ()
glWindowPos3dARB = ffidoubledoubledoubleIOV glWindowPos3dARBFunPtr

glWindowPos3dARBFunPtr :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
glWindowPos3dARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3dARB")

{-# NOINLINE glWindowPos3dARBFunPtr #-}

-- | Usage: @'glWindowPos3dvARB' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3dv'.


glWindowPos3dvARB :: MonadIO m => Ptr GLdouble -> m ()
glWindowPos3dvARB = ffiPtrdoubleIOV glWindowPos3dvARBFunPtr

glWindowPos3dvARBFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glWindowPos3dvARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3dvARB")

{-# NOINLINE glWindowPos3dvARBFunPtr #-}

-- | Usage: @'glWindowPos3fARB' x y z@
--
-- The parameter @x@ is a @CoordF@.
--
-- The parameter @y@ is a @CoordF@.
--
-- The parameter @z@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos3fvARB'.


glWindowPos3fARB :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
glWindowPos3fARB = ffifloatfloatfloatIOV glWindowPos3fARBFunPtr

glWindowPos3fARBFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
glWindowPos3fARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3fARB")

{-# NOINLINE glWindowPos3fARBFunPtr #-}

-- | Usage: @'glWindowPos3fvARB' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3fv'.


glWindowPos3fvARB :: MonadIO m => Ptr GLfloat -> m ()
glWindowPos3fvARB = ffiPtrfloatIOV glWindowPos3fvARBFunPtr

glWindowPos3fvARBFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glWindowPos3fvARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3fvARB")

{-# NOINLINE glWindowPos3fvARBFunPtr #-}

-- | Usage: @'glWindowPos3iARB' x y z@
--
-- The parameter @x@ is a @CoordI@.
--
-- The parameter @y@ is a @CoordI@.
--
-- The parameter @z@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos3ivARB'.


glWindowPos3iARB :: MonadIO m => GLint -> GLint -> GLint -> m ()
glWindowPos3iARB = ffiintintintIOV glWindowPos3iARBFunPtr

glWindowPos3iARBFunPtr :: FunPtr (GLint -> GLint -> GLint -> IO ())
glWindowPos3iARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3iARB")

{-# NOINLINE glWindowPos3iARBFunPtr #-}

-- | Usage: @'glWindowPos3ivARB' v@
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3iv'.


glWindowPos3ivARB :: MonadIO m => Ptr GLint -> m ()
glWindowPos3ivARB = ffiPtrintIOV glWindowPos3ivARBFunPtr

glWindowPos3ivARBFunPtr :: FunPtr (Ptr GLint -> IO ())
glWindowPos3ivARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3ivARB")

{-# NOINLINE glWindowPos3ivARBFunPtr #-}

-- | Usage: @'glWindowPos3sARB' x y z@
--
-- The parameter @x@ is a @CoordS@.
--
-- The parameter @y@ is a @CoordS@.
--
-- The parameter @z@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.ARB.WindowPos.glWindowPos3svARB'.


glWindowPos3sARB :: MonadIO m => GLshort -> GLshort -> GLshort -> m ()
glWindowPos3sARB = ffishortshortshortIOV glWindowPos3sARBFunPtr

glWindowPos3sARBFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
glWindowPos3sARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3sARB")

{-# NOINLINE glWindowPos3sARBFunPtr #-}

-- | Usage: @'glWindowPos3svARB' v@
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3sv'.


glWindowPos3svARB :: MonadIO m => Ptr GLshort -> m ()
glWindowPos3svARB = ffiPtrshortIOV glWindowPos3svARBFunPtr

glWindowPos3svARBFunPtr :: FunPtr (Ptr GLshort -> IO ())
glWindowPos3svARBFunPtr = unsafePerformIO (getProcAddress "glWindowPos3svARB")

{-# NOINLINE glWindowPos3svARBFunPtr #-}