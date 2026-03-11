-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUN.Vertex (
  -- * Extension Support
    gl_SUN_vertex

  -- * GL_SUN_vertex
  , glColor3fVertex3fSUN
  , glColor3fVertex3fvSUN
  , glColor4fNormal3fVertex3fSUN
  , glColor4fNormal3fVertex3fvSUN
  , glColor4ubVertex2fSUN
  , glColor4ubVertex2fvSUN
  , glColor4ubVertex3fSUN
  , glColor4ubVertex3fvSUN
  , glNormal3fVertex3fSUN
  , glNormal3fVertex3fvSUN
  , glReplacementCodeuiColor3fVertex3fSUN
  , glReplacementCodeuiColor3fVertex3fvSUN
  , glReplacementCodeuiColor4fNormal3fVertex3fSUN
  , glReplacementCodeuiColor4fNormal3fVertex3fvSUN
  , glReplacementCodeuiColor4ubVertex3fSUN
  , glReplacementCodeuiColor4ubVertex3fvSUN
  , glReplacementCodeuiNormal3fVertex3fSUN
  , glReplacementCodeuiNormal3fVertex3fvSUN
  , glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN
  , glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN
  , glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN
  , glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN
  , glReplacementCodeuiTexCoord2fVertex3fSUN
  , glReplacementCodeuiTexCoord2fVertex3fvSUN
  , glReplacementCodeuiVertex3fSUN
  , glReplacementCodeuiVertex3fvSUN
  , glTexCoord2fColor3fVertex3fSUN
  , glTexCoord2fColor3fVertex3fvSUN
  , glTexCoord2fColor4fNormal3fVertex3fSUN
  , glTexCoord2fColor4fNormal3fVertex3fvSUN
  , glTexCoord2fColor4ubVertex3fSUN
  , glTexCoord2fColor4ubVertex3fvSUN
  , glTexCoord2fNormal3fVertex3fSUN
  , glTexCoord2fNormal3fVertex3fvSUN
  , glTexCoord2fVertex3fSUN
  , glTexCoord2fVertex3fvSUN
  , glTexCoord4fColor4fNormal3fVertex4fSUN
  , glTexCoord4fColor4fNormal3fVertex4fvSUN
  , glTexCoord4fVertex4fSUN
  , glTexCoord4fVertex4fvSUN
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUN/vertex.txt GL_SUN_vertex> extension is available.

gl_SUN_vertex :: Bool
gl_SUN_vertex = member "GL_SUN_vertex" extensions
{-# NOINLINE gl_SUN_vertex #-}

-- | Usage: @'glColor3fVertex3fSUN' r g b x y z@


glColor3fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glColor3fVertex3fSUN = ffifloatfloatfloatfloatfloatfloatIOV glColor3fVertex3fSUNFunPtr

glColor3fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glColor3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glColor3fVertex3fSUN")

{-# NOINLINE glColor3fVertex3fSUNFunPtr #-}

-- | Usage: @'glColor3fVertex3fvSUN' c v@
--
-- The length of @c@ should be @3@.
--
-- The length of @v@ should be @3@.


glColor3fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> m ()
glColor3fVertex3fvSUN = ffiPtrfloatPtrfloatIOV glColor3fVertex3fvSUNFunPtr

glColor3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
glColor3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glColor3fVertex3fvSUN")

{-# NOINLINE glColor3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glColor4fNormal3fVertex3fSUN' r g b a nx ny nz x y z@


glColor4fNormal3fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glColor4fNormal3fVertex3fSUN = ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV glColor4fNormal3fVertex3fSUNFunPtr

glColor4fNormal3fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glColor4fNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glColor4fNormal3fVertex3fSUN")

{-# NOINLINE glColor4fNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glColor4fNormal3fVertex3fvSUN' c n v@
--
-- The length of @c@ should be @4@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glColor4fNormal3fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glColor4fNormal3fVertex3fvSUN = ffiPtrfloatPtrfloatPtrfloatIOV glColor4fNormal3fVertex3fvSUNFunPtr

glColor4fNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glColor4fNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glColor4fNormal3fVertex3fvSUN")

{-# NOINLINE glColor4fNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glColor4ubVertex2fSUN' r g b a x y@


glColor4ubVertex2fSUN :: MonadIO m => GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> m ()
glColor4ubVertex2fSUN = ffiubyteubyteubyteubytefloatfloatIOV glColor4ubVertex2fSUNFunPtr

glColor4ubVertex2fSUNFunPtr :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> IO ())
glColor4ubVertex2fSUNFunPtr = unsafePerformIO (getProcAddress "glColor4ubVertex2fSUN")

{-# NOINLINE glColor4ubVertex2fSUNFunPtr #-}

-- | Usage: @'glColor4ubVertex2fvSUN' c v@
--
-- The length of @c@ should be @4@.
--
-- The length of @v@ should be @2@.


glColor4ubVertex2fvSUN :: MonadIO m => Ptr GLubyte -> Ptr GLfloat -> m ()
glColor4ubVertex2fvSUN = ffiPtrubytePtrfloatIOV glColor4ubVertex2fvSUNFunPtr

glColor4ubVertex2fvSUNFunPtr :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ())
glColor4ubVertex2fvSUNFunPtr = unsafePerformIO (getProcAddress "glColor4ubVertex2fvSUN")

{-# NOINLINE glColor4ubVertex2fvSUNFunPtr #-}

-- | Usage: @'glColor4ubVertex3fSUN' r g b a x y z@


glColor4ubVertex3fSUN :: MonadIO m => GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> m ()
glColor4ubVertex3fSUN = ffiubyteubyteubyteubytefloatfloatfloatIOV glColor4ubVertex3fSUNFunPtr

glColor4ubVertex3fSUNFunPtr :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
glColor4ubVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glColor4ubVertex3fSUN")

{-# NOINLINE glColor4ubVertex3fSUNFunPtr #-}

-- | Usage: @'glColor4ubVertex3fvSUN' c v@
--
-- The length of @c@ should be @4@.
--
-- The length of @v@ should be @3@.


glColor4ubVertex3fvSUN :: MonadIO m => Ptr GLubyte -> Ptr GLfloat -> m ()
glColor4ubVertex3fvSUN = ffiPtrubytePtrfloatIOV glColor4ubVertex3fvSUNFunPtr

glColor4ubVertex3fvSUNFunPtr :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ())
glColor4ubVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glColor4ubVertex3fvSUN")

{-# NOINLINE glColor4ubVertex3fvSUNFunPtr #-}

-- | Usage: @'glNormal3fVertex3fSUN' nx ny nz x y z@


glNormal3fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glNormal3fVertex3fSUN = ffifloatfloatfloatfloatfloatfloatIOV glNormal3fVertex3fSUNFunPtr

glNormal3fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glNormal3fVertex3fSUN")

{-# NOINLINE glNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glNormal3fVertex3fvSUN' n v@
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glNormal3fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> m ()
glNormal3fVertex3fvSUN = ffiPtrfloatPtrfloatIOV glNormal3fVertex3fvSUNFunPtr

glNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
glNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glNormal3fVertex3fvSUN")

{-# NOINLINE glNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiColor3fVertex3fSUN' rc r g b x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiColor3fVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiColor3fVertex3fSUN = ffiuintfloatfloatfloatfloatfloatfloatIOV glReplacementCodeuiColor3fVertex3fSUNFunPtr

glReplacementCodeuiColor3fVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiColor3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiColor3fVertex3fSUN")

{-# NOINLINE glReplacementCodeuiColor3fVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiColor3fVertex3fvSUN' rc c v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @c@ should be @3@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiColor3fVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> m ()
glReplacementCodeuiColor3fVertex3fvSUN = ffiPtruintPtrfloatPtrfloatIOV glReplacementCodeuiColor3fVertex3fvSUNFunPtr

glReplacementCodeuiColor3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glReplacementCodeuiColor3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiColor3fVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiColor3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiColor4fNormal3fVertex3fSUN' rc r g b a nx ny nz x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiColor4fNormal3fVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiColor4fNormal3fVertex3fSUN = ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV glReplacementCodeuiColor4fNormal3fVertex3fSUNFunPtr

glReplacementCodeuiColor4fNormal3fVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiColor4fNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiColor4fNormal3fVertex3fSUN")

{-# NOINLINE glReplacementCodeuiColor4fNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiColor4fNormal3fVertex3fvSUN' rc c n v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @c@ should be @4@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiColor4fNormal3fVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glReplacementCodeuiColor4fNormal3fVertex3fvSUN = ffiPtruintPtrfloatPtrfloatPtrfloatIOV glReplacementCodeuiColor4fNormal3fVertex3fvSUNFunPtr

glReplacementCodeuiColor4fNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glReplacementCodeuiColor4fNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiColor4fNormal3fVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiColor4fNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiColor4ubVertex3fSUN' rc r g b a x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiColor4ubVertex3fSUN :: MonadIO m => GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiColor4ubVertex3fSUN = ffiuintubyteubyteubyteubytefloatfloatfloatIOV glReplacementCodeuiColor4ubVertex3fSUNFunPtr

glReplacementCodeuiColor4ubVertex3fSUNFunPtr :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiColor4ubVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiColor4ubVertex3fSUN")

{-# NOINLINE glReplacementCodeuiColor4ubVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiColor4ubVertex3fvSUN' rc c v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @c@ should be @4@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiColor4ubVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> m ()
glReplacementCodeuiColor4ubVertex3fvSUN = ffiPtruintPtrubytePtrfloatIOV glReplacementCodeuiColor4ubVertex3fvSUNFunPtr

glReplacementCodeuiColor4ubVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> IO ())
glReplacementCodeuiColor4ubVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiColor4ubVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiColor4ubVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiNormal3fVertex3fSUN' rc nx ny nz x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiNormal3fVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiNormal3fVertex3fSUN = ffiuintfloatfloatfloatfloatfloatfloatIOV glReplacementCodeuiNormal3fVertex3fSUNFunPtr

glReplacementCodeuiNormal3fVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiNormal3fVertex3fSUN")

{-# NOINLINE glReplacementCodeuiNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiNormal3fVertex3fvSUN' rc n v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiNormal3fVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> m ()
glReplacementCodeuiNormal3fVertex3fvSUN = ffiPtruintPtrfloatPtrfloatIOV glReplacementCodeuiNormal3fVertex3fvSUNFunPtr

glReplacementCodeuiNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glReplacementCodeuiNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiNormal3fVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN' rc s t r g b a nx ny nz x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN = ffiuintfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUNFunPtr

glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN")

{-# NOINLINE glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN' rc tc c n v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @tc@ should be @2@.
--
-- The length of @c@ should be @4@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN = ffiPtruintPtrfloatPtrfloatPtrfloatPtrfloatIOV glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr

glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN' rc s t nx ny nz x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN = ffiuintfloatfloatfloatfloatfloatfloatfloatfloatIOV glReplacementCodeuiTexCoord2fNormal3fVertex3fSUNFunPtr

glReplacementCodeuiTexCoord2fNormal3fVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiTexCoord2fNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN")

{-# NOINLINE glReplacementCodeuiTexCoord2fNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN' rc tc n v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @tc@ should be @2@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN = ffiPtruintPtrfloatPtrfloatPtrfloatIOV glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUNFunPtr

glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiTexCoord2fVertex3fSUN' rc s t x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiTexCoord2fVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiTexCoord2fVertex3fSUN = ffiuintfloatfloatfloatfloatfloatIOV glReplacementCodeuiTexCoord2fVertex3fSUNFunPtr

glReplacementCodeuiTexCoord2fVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiTexCoord2fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiTexCoord2fVertex3fSUN")

{-# NOINLINE glReplacementCodeuiTexCoord2fVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiTexCoord2fVertex3fvSUN' rc tc v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @tc@ should be @2@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiTexCoord2fVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> m ()
glReplacementCodeuiTexCoord2fVertex3fvSUN = ffiPtruintPtrfloatPtrfloatIOV glReplacementCodeuiTexCoord2fVertex3fvSUNFunPtr

glReplacementCodeuiTexCoord2fVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glReplacementCodeuiTexCoord2fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiTexCoord2fVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiTexCoord2fVertex3fvSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiVertex3fSUN' rc x y z@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.


glReplacementCodeuiVertex3fSUN :: MonadIO m => GLuint -> GLfloat -> GLfloat -> GLfloat -> m ()
glReplacementCodeuiVertex3fSUN = ffiuintfloatfloatfloatIOV glReplacementCodeuiVertex3fSUNFunPtr

glReplacementCodeuiVertex3fSUNFunPtr :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
glReplacementCodeuiVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiVertex3fSUN")

{-# NOINLINE glReplacementCodeuiVertex3fSUNFunPtr #-}

-- | Usage: @'glReplacementCodeuiVertex3fvSUN' rc v@
--
-- The parameter @rc@ is a @ReplacementCodeSUN@.
--
-- The length of @rc@ should be @1@.
--
-- The length of @v@ should be @3@.


glReplacementCodeuiVertex3fvSUN :: MonadIO m => Ptr GLuint -> Ptr GLfloat -> m ()
glReplacementCodeuiVertex3fvSUN = ffiPtruintPtrfloatIOV glReplacementCodeuiVertex3fvSUNFunPtr

glReplacementCodeuiVertex3fvSUNFunPtr :: FunPtr (Ptr GLuint -> Ptr GLfloat -> IO ())
glReplacementCodeuiVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glReplacementCodeuiVertex3fvSUN")

{-# NOINLINE glReplacementCodeuiVertex3fvSUNFunPtr #-}

-- | Usage: @'glTexCoord2fColor3fVertex3fSUN' s t r g b x y z@


glTexCoord2fColor3fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord2fColor3fVertex3fSUN = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glTexCoord2fColor3fVertex3fSUNFunPtr

glTexCoord2fColor3fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord2fColor3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fColor3fVertex3fSUN")

{-# NOINLINE glTexCoord2fColor3fVertex3fSUNFunPtr #-}

-- | Usage: @'glTexCoord2fColor3fVertex3fvSUN' tc c v@
--
-- The length of @tc@ should be @2@.
--
-- The length of @c@ should be @3@.
--
-- The length of @v@ should be @3@.


glTexCoord2fColor3fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glTexCoord2fColor3fVertex3fvSUN = ffiPtrfloatPtrfloatPtrfloatIOV glTexCoord2fColor3fVertex3fvSUNFunPtr

glTexCoord2fColor3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glTexCoord2fColor3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fColor3fVertex3fvSUN")

{-# NOINLINE glTexCoord2fColor3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glTexCoord2fColor4fNormal3fVertex3fSUN' s t r g b a nx ny nz x y z@


glTexCoord2fColor4fNormal3fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord2fColor4fNormal3fVertex3fSUN = ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV glTexCoord2fColor4fNormal3fVertex3fSUNFunPtr

glTexCoord2fColor4fNormal3fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord2fColor4fNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fColor4fNormal3fVertex3fSUN")

{-# NOINLINE glTexCoord2fColor4fNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glTexCoord2fColor4fNormal3fVertex3fvSUN' tc c n v@
--
-- The length of @tc@ should be @2@.
--
-- The length of @c@ should be @4@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glTexCoord2fColor4fNormal3fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glTexCoord2fColor4fNormal3fVertex3fvSUN = ffiPtrfloatPtrfloatPtrfloatPtrfloatIOV glTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr

glTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fColor4fNormal3fVertex3fvSUN")

{-# NOINLINE glTexCoord2fColor4fNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glTexCoord2fColor4ubVertex3fSUN' s t r g b a x y z@


glTexCoord2fColor4ubVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord2fColor4ubVertex3fSUN = ffifloatfloatubyteubyteubyteubytefloatfloatfloatIOV glTexCoord2fColor4ubVertex3fSUNFunPtr

glTexCoord2fColor4ubVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord2fColor4ubVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fColor4ubVertex3fSUN")

{-# NOINLINE glTexCoord2fColor4ubVertex3fSUNFunPtr #-}

-- | Usage: @'glTexCoord2fColor4ubVertex3fvSUN' tc c v@
--
-- The length of @tc@ should be @2@.
--
-- The length of @c@ should be @4@.
--
-- The length of @v@ should be @3@.


glTexCoord2fColor4ubVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> m ()
glTexCoord2fColor4ubVertex3fvSUN = ffiPtrfloatPtrubytePtrfloatIOV glTexCoord2fColor4ubVertex3fvSUNFunPtr

glTexCoord2fColor4ubVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> IO ())
glTexCoord2fColor4ubVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fColor4ubVertex3fvSUN")

{-# NOINLINE glTexCoord2fColor4ubVertex3fvSUNFunPtr #-}

-- | Usage: @'glTexCoord2fNormal3fVertex3fSUN' s t nx ny nz x y z@


glTexCoord2fNormal3fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord2fNormal3fVertex3fSUN = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glTexCoord2fNormal3fVertex3fSUNFunPtr

glTexCoord2fNormal3fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord2fNormal3fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fNormal3fVertex3fSUN")

{-# NOINLINE glTexCoord2fNormal3fVertex3fSUNFunPtr #-}

-- | Usage: @'glTexCoord2fNormal3fVertex3fvSUN' tc n v@
--
-- The length of @tc@ should be @2@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @3@.


glTexCoord2fNormal3fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glTexCoord2fNormal3fVertex3fvSUN = ffiPtrfloatPtrfloatPtrfloatIOV glTexCoord2fNormal3fVertex3fvSUNFunPtr

glTexCoord2fNormal3fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glTexCoord2fNormal3fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fNormal3fVertex3fvSUN")

{-# NOINLINE glTexCoord2fNormal3fVertex3fvSUNFunPtr #-}

-- | Usage: @'glTexCoord2fVertex3fSUN' s t x y z@


glTexCoord2fVertex3fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord2fVertex3fSUN = ffifloatfloatfloatfloatfloatIOV glTexCoord2fVertex3fSUNFunPtr

glTexCoord2fVertex3fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord2fVertex3fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fVertex3fSUN")

{-# NOINLINE glTexCoord2fVertex3fSUNFunPtr #-}

-- | Usage: @'glTexCoord2fVertex3fvSUN' tc v@
--
-- The length of @tc@ should be @2@.
--
-- The length of @v@ should be @3@.


glTexCoord2fVertex3fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> m ()
glTexCoord2fVertex3fvSUN = ffiPtrfloatPtrfloatIOV glTexCoord2fVertex3fvSUNFunPtr

glTexCoord2fVertex3fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
glTexCoord2fVertex3fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord2fVertex3fvSUN")

{-# NOINLINE glTexCoord2fVertex3fvSUNFunPtr #-}

-- | Usage: @'glTexCoord4fColor4fNormal3fVertex4fSUN' s t p q r g b a nx ny nz x y z w@


glTexCoord4fColor4fNormal3fVertex4fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord4fColor4fNormal3fVertex4fSUN = ffifloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatfloatIOV glTexCoord4fColor4fNormal3fVertex4fSUNFunPtr

glTexCoord4fColor4fNormal3fVertex4fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord4fColor4fNormal3fVertex4fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord4fColor4fNormal3fVertex4fSUN")

{-# NOINLINE glTexCoord4fColor4fNormal3fVertex4fSUNFunPtr #-}

-- | Usage: @'glTexCoord4fColor4fNormal3fVertex4fvSUN' tc c n v@
--
-- The length of @tc@ should be @4@.
--
-- The length of @c@ should be @4@.
--
-- The length of @n@ should be @3@.
--
-- The length of @v@ should be @4@.


glTexCoord4fColor4fNormal3fVertex4fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> m ()
glTexCoord4fColor4fNormal3fVertex4fvSUN = ffiPtrfloatPtrfloatPtrfloatPtrfloatIOV glTexCoord4fColor4fNormal3fVertex4fvSUNFunPtr

glTexCoord4fColor4fNormal3fVertex4fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
glTexCoord4fColor4fNormal3fVertex4fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord4fColor4fNormal3fVertex4fvSUN")

{-# NOINLINE glTexCoord4fColor4fNormal3fVertex4fvSUNFunPtr #-}

-- | Usage: @'glTexCoord4fVertex4fSUN' s t p q x y z w@


glTexCoord4fVertex4fSUN :: MonadIO m => GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> m ()
glTexCoord4fVertex4fSUN = ffifloatfloatfloatfloatfloatfloatfloatfloatIOV glTexCoord4fVertex4fSUNFunPtr

glTexCoord4fVertex4fSUNFunPtr :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
glTexCoord4fVertex4fSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord4fVertex4fSUN")

{-# NOINLINE glTexCoord4fVertex4fSUNFunPtr #-}

-- | Usage: @'glTexCoord4fVertex4fvSUN' tc v@
--
-- The length of @tc@ should be @4@.
--
-- The length of @v@ should be @4@.


glTexCoord4fVertex4fvSUN :: MonadIO m => Ptr GLfloat -> Ptr GLfloat -> m ()
glTexCoord4fVertex4fvSUN = ffiPtrfloatPtrfloatIOV glTexCoord4fVertex4fvSUNFunPtr

glTexCoord4fVertex4fvSUNFunPtr :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
glTexCoord4fVertex4fvSUNFunPtr = unsafePerformIO (getProcAddress "glTexCoord4fVertex4fvSUN")

{-# NOINLINE glTexCoord4fVertex4fvSUNFunPtr #-}