-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.MESA.WindowPos (
  -- * Extension Support
    gl_MESA_window_pos

  -- * GL_MESA_window_pos
  , glWindowPos2dMESA
  , glWindowPos2dvMESA
  , glWindowPos2fMESA
  , glWindowPos2fvMESA
  , glWindowPos2iMESA
  , glWindowPos2ivMESA
  , glWindowPos2sMESA
  , glWindowPos2svMESA
  , glWindowPos3dMESA
  , glWindowPos3dvMESA
  , glWindowPos3fMESA
  , glWindowPos3fvMESA
  , glWindowPos3iMESA
  , glWindowPos3ivMESA
  , glWindowPos3sMESA
  , glWindowPos3svMESA
  , glWindowPos4dMESA
  , glWindowPos4dvMESA
  , glWindowPos4fMESA
  , glWindowPos4fvMESA
  , glWindowPos4iMESA
  , glWindowPos4ivMESA
  , glWindowPos4sMESA
  , glWindowPos4svMESA
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/MESA/window_pos.txt GL_MESA_window_pos> extension is available.

gl_MESA_window_pos :: Bool
gl_MESA_window_pos = member "GL_MESA_window_pos" extensions
{-# NOINLINE gl_MESA_window_pos #-}

-- | Usage: @'glWindowPos2dMESA' x y@
--
-- The parameter @x@ is a @CoordD@.
--
-- The parameter @y@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos2dvMESA'.


glWindowPos2dMESA :: MonadIO m => GLdouble -> GLdouble -> m ()
glWindowPos2dMESA = ffidoubledoubleIOV glWindowPos2dMESAFunPtr

glWindowPos2dMESAFunPtr :: FunPtr (GLdouble -> GLdouble -> IO ())
glWindowPos2dMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2dMESA")

{-# NOINLINE glWindowPos2dMESAFunPtr #-}

-- | Usage: @'glWindowPos2dvMESA' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2dv'.


glWindowPos2dvMESA :: MonadIO m => Ptr GLdouble -> m ()
glWindowPos2dvMESA = ffiPtrdoubleIOV glWindowPos2dvMESAFunPtr

glWindowPos2dvMESAFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glWindowPos2dvMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2dvMESA")

{-# NOINLINE glWindowPos2dvMESAFunPtr #-}

-- | Usage: @'glWindowPos2fMESA' x y@
--
-- The parameter @x@ is a @CoordF@.
--
-- The parameter @y@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos2fvMESA'.


glWindowPos2fMESA :: MonadIO m => GLfloat -> GLfloat -> m ()
glWindowPos2fMESA = ffifloatfloatIOV glWindowPos2fMESAFunPtr

glWindowPos2fMESAFunPtr :: FunPtr (GLfloat -> GLfloat -> IO ())
glWindowPos2fMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2fMESA")

{-# NOINLINE glWindowPos2fMESAFunPtr #-}

-- | Usage: @'glWindowPos2fvMESA' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2fv'.


glWindowPos2fvMESA :: MonadIO m => Ptr GLfloat -> m ()
glWindowPos2fvMESA = ffiPtrfloatIOV glWindowPos2fvMESAFunPtr

glWindowPos2fvMESAFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glWindowPos2fvMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2fvMESA")

{-# NOINLINE glWindowPos2fvMESAFunPtr #-}

-- | Usage: @'glWindowPos2iMESA' x y@
--
-- The parameter @x@ is a @CoordI@.
--
-- The parameter @y@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos2ivMESA'.


glWindowPos2iMESA :: MonadIO m => GLint -> GLint -> m ()
glWindowPos2iMESA = ffiintintIOV glWindowPos2iMESAFunPtr

glWindowPos2iMESAFunPtr :: FunPtr (GLint -> GLint -> IO ())
glWindowPos2iMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2iMESA")

{-# NOINLINE glWindowPos2iMESAFunPtr #-}

-- | Usage: @'glWindowPos2ivMESA' v@
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2iv'.


glWindowPos2ivMESA :: MonadIO m => Ptr GLint -> m ()
glWindowPos2ivMESA = ffiPtrintIOV glWindowPos2ivMESAFunPtr

glWindowPos2ivMESAFunPtr :: FunPtr (Ptr GLint -> IO ())
glWindowPos2ivMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2ivMESA")

{-# NOINLINE glWindowPos2ivMESAFunPtr #-}

-- | Usage: @'glWindowPos2sMESA' x y@
--
-- The parameter @x@ is a @CoordS@.
--
-- The parameter @y@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos2svMESA'.


glWindowPos2sMESA :: MonadIO m => GLshort -> GLshort -> m ()
glWindowPos2sMESA = ffishortshortIOV glWindowPos2sMESAFunPtr

glWindowPos2sMESAFunPtr :: FunPtr (GLshort -> GLshort -> IO ())
glWindowPos2sMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2sMESA")

{-# NOINLINE glWindowPos2sMESAFunPtr #-}

-- | Usage: @'glWindowPos2svMESA' v@
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @2@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos2sv'.


glWindowPos2svMESA :: MonadIO m => Ptr GLshort -> m ()
glWindowPos2svMESA = ffiPtrshortIOV glWindowPos2svMESAFunPtr

glWindowPos2svMESAFunPtr :: FunPtr (Ptr GLshort -> IO ())
glWindowPos2svMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos2svMESA")

{-# NOINLINE glWindowPos2svMESAFunPtr #-}

-- | Usage: @'glWindowPos3dMESA' x y z@
--
-- The parameter @x@ is a @CoordD@.
--
-- The parameter @y@ is a @CoordD@.
--
-- The parameter @z@ is a @CoordD@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3d'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos3dvMESA'.


glWindowPos3dMESA :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> m ()
glWindowPos3dMESA = ffidoubledoubledoubleIOV glWindowPos3dMESAFunPtr

glWindowPos3dMESAFunPtr :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
glWindowPos3dMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3dMESA")

{-# NOINLINE glWindowPos3dMESAFunPtr #-}

-- | Usage: @'glWindowPos3dvMESA' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3dv'.


glWindowPos3dvMESA :: MonadIO m => Ptr GLdouble -> m ()
glWindowPos3dvMESA = ffiPtrdoubleIOV glWindowPos3dvMESAFunPtr

glWindowPos3dvMESAFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glWindowPos3dvMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3dvMESA")

{-# NOINLINE glWindowPos3dvMESAFunPtr #-}

-- | Usage: @'glWindowPos3fMESA' x y z@
--
-- The parameter @x@ is a @CoordF@.
--
-- The parameter @y@ is a @CoordF@.
--
-- The parameter @z@ is a @CoordF@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3f'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos3fvMESA'.


glWindowPos3fMESA :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> m ()
glWindowPos3fMESA = ffifloatfloatfloatIOV glWindowPos3fMESAFunPtr

glWindowPos3fMESAFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
glWindowPos3fMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3fMESA")

{-# NOINLINE glWindowPos3fMESAFunPtr #-}

-- | Usage: @'glWindowPos3fvMESA' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3fv'.


glWindowPos3fvMESA :: MonadIO m => Ptr GLfloat -> m ()
glWindowPos3fvMESA = ffiPtrfloatIOV glWindowPos3fvMESAFunPtr

glWindowPos3fvMESAFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glWindowPos3fvMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3fvMESA")

{-# NOINLINE glWindowPos3fvMESAFunPtr #-}

-- | Usage: @'glWindowPos3iMESA' x y z@
--
-- The parameter @x@ is a @CoordI@.
--
-- The parameter @y@ is a @CoordI@.
--
-- The parameter @z@ is a @CoordI@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3i'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos3ivMESA'.


glWindowPos3iMESA :: MonadIO m => GLint -> GLint -> GLint -> m ()
glWindowPos3iMESA = ffiintintintIOV glWindowPos3iMESAFunPtr

glWindowPos3iMESAFunPtr :: FunPtr (GLint -> GLint -> GLint -> IO ())
glWindowPos3iMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3iMESA")

{-# NOINLINE glWindowPos3iMESAFunPtr #-}

-- | Usage: @'glWindowPos3ivMESA' v@
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3iv'.


glWindowPos3ivMESA :: MonadIO m => Ptr GLint -> m ()
glWindowPos3ivMESA = ffiPtrintIOV glWindowPos3ivMESAFunPtr

glWindowPos3ivMESAFunPtr :: FunPtr (Ptr GLint -> IO ())
glWindowPos3ivMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3ivMESA")

{-# NOINLINE glWindowPos3ivMESAFunPtr #-}

-- | Usage: @'glWindowPos3sMESA' x y z@
--
-- The parameter @x@ is a @CoordS@.
--
-- The parameter @y@ is a @CoordS@.
--
-- The parameter @z@ is a @CoordS@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3s'.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos3svMESA'.


glWindowPos3sMESA :: MonadIO m => GLshort -> GLshort -> GLshort -> m ()
glWindowPos3sMESA = ffishortshortshortIOV glWindowPos3sMESAFunPtr

glWindowPos3sMESAFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
glWindowPos3sMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3sMESA")

{-# NOINLINE glWindowPos3sMESAFunPtr #-}

-- | Usage: @'glWindowPos3svMESA' v@
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @3@.
--
-- This command is an alias for 'Graphics.GL.Internal.Shared.glWindowPos3sv'.


glWindowPos3svMESA :: MonadIO m => Ptr GLshort -> m ()
glWindowPos3svMESA = ffiPtrshortIOV glWindowPos3svMESAFunPtr

glWindowPos3svMESAFunPtr :: FunPtr (Ptr GLshort -> IO ())
glWindowPos3svMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos3svMESA")

{-# NOINLINE glWindowPos3svMESAFunPtr #-}

-- | Usage: @'glWindowPos4dMESA' x y z w@
--
-- The parameter @x@ is a @CoordD@.
--
-- The parameter @y@ is a @CoordD@.
--
-- The parameter @z@ is a @CoordD@.
--
-- The parameter @w@ is a @CoordD@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos4dvMESA'.


glWindowPos4dMESA :: MonadIO m => GLdouble -> GLdouble -> GLdouble -> GLdouble -> m ()
glWindowPos4dMESA = ffidoubledoubledoubledoubleIOV glWindowPos4dMESAFunPtr

glWindowPos4dMESAFunPtr :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
glWindowPos4dMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4dMESA")

{-# NOINLINE glWindowPos4dMESAFunPtr #-}

-- | Usage: @'glWindowPos4dvMESA' v@
--
-- The parameter @v@ is a @CoordD@.
--
-- The length of @v@ should be @4@.


glWindowPos4dvMESA :: MonadIO m => Ptr GLdouble -> m ()
glWindowPos4dvMESA = ffiPtrdoubleIOV glWindowPos4dvMESAFunPtr

glWindowPos4dvMESAFunPtr :: FunPtr (Ptr GLdouble -> IO ())
glWindowPos4dvMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4dvMESA")

{-# NOINLINE glWindowPos4dvMESAFunPtr #-}

-- | Usage: @'glWindowPos4fMESA' x y z w@
--
-- The parameter @x@ is a @CoordF@.
--
-- The parameter @y@ is a @CoordF@.
--
-- The parameter @z@ is a @CoordF@.
--
-- The parameter @w@ is a @CoordF@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos4fvMESA'.


glWindowPos4fMESA :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glWindowPos4fMESA = ffifloatfloatfloatfloatIOV glWindowPos4fMESAFunPtr

glWindowPos4fMESAFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glWindowPos4fMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4fMESA")

{-# NOINLINE glWindowPos4fMESAFunPtr #-}

-- | Usage: @'glWindowPos4fvMESA' v@
--
-- The parameter @v@ is a @CoordF@.
--
-- The length of @v@ should be @4@.


glWindowPos4fvMESA :: MonadIO m => Ptr GLfloat -> m ()
glWindowPos4fvMESA = ffiPtrfloatIOV glWindowPos4fvMESAFunPtr

glWindowPos4fvMESAFunPtr :: FunPtr (Ptr GLfloat -> IO ())
glWindowPos4fvMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4fvMESA")

{-# NOINLINE glWindowPos4fvMESAFunPtr #-}

-- | Usage: @'glWindowPos4iMESA' x y z w@
--
-- The parameter @x@ is a @CoordI@.
--
-- The parameter @y@ is a @CoordI@.
--
-- The parameter @z@ is a @CoordI@.
--
-- The parameter @w@ is a @CoordI@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos4ivMESA'.


glWindowPos4iMESA :: MonadIO m => GLint -> GLint -> GLint -> GLint -> m ()
glWindowPos4iMESA = ffiintintintintIOV glWindowPos4iMESAFunPtr

glWindowPos4iMESAFunPtr :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
glWindowPos4iMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4iMESA")

{-# NOINLINE glWindowPos4iMESAFunPtr #-}

-- | Usage: @'glWindowPos4ivMESA' v@
--
-- The parameter @v@ is a @CoordI@.
--
-- The length of @v@ should be @4@.


glWindowPos4ivMESA :: MonadIO m => Ptr GLint -> m ()
glWindowPos4ivMESA = ffiPtrintIOV glWindowPos4ivMESAFunPtr

glWindowPos4ivMESAFunPtr :: FunPtr (Ptr GLint -> IO ())
glWindowPos4ivMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4ivMESA")

{-# NOINLINE glWindowPos4ivMESAFunPtr #-}

-- | Usage: @'glWindowPos4sMESA' x y z w@
--
-- The parameter @x@ is a @CoordS@.
--
-- The parameter @y@ is a @CoordS@.
--
-- The parameter @z@ is a @CoordS@.
--
-- The parameter @w@ is a @CoordS@.
--
-- The vector equivalent of this command is 'Graphics.GL.Ext.MESA.WindowPos.glWindowPos4svMESA'.


glWindowPos4sMESA :: MonadIO m => GLshort -> GLshort -> GLshort -> GLshort -> m ()
glWindowPos4sMESA = ffishortshortshortshortIOV glWindowPos4sMESAFunPtr

glWindowPos4sMESAFunPtr :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
glWindowPos4sMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4sMESA")

{-# NOINLINE glWindowPos4sMESAFunPtr #-}

-- | Usage: @'glWindowPos4svMESA' v@
--
-- The parameter @v@ is a @CoordS@.
--
-- The length of @v@ should be @4@.


glWindowPos4svMESA :: MonadIO m => Ptr GLshort -> m ()
glWindowPos4svMESA = ffiPtrshortIOV glWindowPos4svMESAFunPtr

glWindowPos4svMESAFunPtr :: FunPtr (Ptr GLshort -> IO ())
glWindowPos4svMESAFunPtr = unsafePerformIO (getProcAddress "glWindowPos4svMESA")

{-# NOINLINE glWindowPos4svMESAFunPtr #-}