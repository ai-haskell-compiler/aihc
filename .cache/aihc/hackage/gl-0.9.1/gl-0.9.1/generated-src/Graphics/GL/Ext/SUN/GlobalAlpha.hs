-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.SUN.GlobalAlpha (
  -- * Extension Support
    gl_SUN_global_alpha

  -- * GL_SUN_global_alpha
  , glGlobalAlphaFactorbSUN
  , glGlobalAlphaFactordSUN
  , glGlobalAlphaFactorfSUN
  , glGlobalAlphaFactoriSUN
  , glGlobalAlphaFactorsSUN
  , glGlobalAlphaFactorubSUN
  , glGlobalAlphaFactoruiSUN
  , glGlobalAlphaFactorusSUN
  , pattern GL_GLOBAL_ALPHA_FACTOR_SUN
  , pattern GL_GLOBAL_ALPHA_SUN
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/SUN/global_alpha.txt GL_SUN_global_alpha> extension is available.

gl_SUN_global_alpha :: Bool
gl_SUN_global_alpha = member "GL_SUN_global_alpha" extensions
{-# NOINLINE gl_SUN_global_alpha #-}

-- | Usage: @'glGlobalAlphaFactorbSUN' factor@


glGlobalAlphaFactorbSUN :: MonadIO m => GLbyte -> m ()
glGlobalAlphaFactorbSUN = ffibyteIOV glGlobalAlphaFactorbSUNFunPtr

glGlobalAlphaFactorbSUNFunPtr :: FunPtr (GLbyte -> IO ())
glGlobalAlphaFactorbSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactorbSUN")

{-# NOINLINE glGlobalAlphaFactorbSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactordSUN' factor@


glGlobalAlphaFactordSUN :: MonadIO m => GLdouble -> m ()
glGlobalAlphaFactordSUN = ffidoubleIOV glGlobalAlphaFactordSUNFunPtr

glGlobalAlphaFactordSUNFunPtr :: FunPtr (GLdouble -> IO ())
glGlobalAlphaFactordSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactordSUN")

{-# NOINLINE glGlobalAlphaFactordSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactorfSUN' factor@


glGlobalAlphaFactorfSUN :: MonadIO m => GLfloat -> m ()
glGlobalAlphaFactorfSUN = ffifloatIOV glGlobalAlphaFactorfSUNFunPtr

glGlobalAlphaFactorfSUNFunPtr :: FunPtr (GLfloat -> IO ())
glGlobalAlphaFactorfSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactorfSUN")

{-# NOINLINE glGlobalAlphaFactorfSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactoriSUN' factor@


glGlobalAlphaFactoriSUN :: MonadIO m => GLint -> m ()
glGlobalAlphaFactoriSUN = ffiintIOV glGlobalAlphaFactoriSUNFunPtr

glGlobalAlphaFactoriSUNFunPtr :: FunPtr (GLint -> IO ())
glGlobalAlphaFactoriSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactoriSUN")

{-# NOINLINE glGlobalAlphaFactoriSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactorsSUN' factor@


glGlobalAlphaFactorsSUN :: MonadIO m => GLshort -> m ()
glGlobalAlphaFactorsSUN = ffishortIOV glGlobalAlphaFactorsSUNFunPtr

glGlobalAlphaFactorsSUNFunPtr :: FunPtr (GLshort -> IO ())
glGlobalAlphaFactorsSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactorsSUN")

{-# NOINLINE glGlobalAlphaFactorsSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactorubSUN' factor@


glGlobalAlphaFactorubSUN :: MonadIO m => GLubyte -> m ()
glGlobalAlphaFactorubSUN = ffiubyteIOV glGlobalAlphaFactorubSUNFunPtr

glGlobalAlphaFactorubSUNFunPtr :: FunPtr (GLubyte -> IO ())
glGlobalAlphaFactorubSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactorubSUN")

{-# NOINLINE glGlobalAlphaFactorubSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactoruiSUN' factor@


glGlobalAlphaFactoruiSUN :: MonadIO m => GLuint -> m ()
glGlobalAlphaFactoruiSUN = ffiuintIOV glGlobalAlphaFactoruiSUNFunPtr

glGlobalAlphaFactoruiSUNFunPtr :: FunPtr (GLuint -> IO ())
glGlobalAlphaFactoruiSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactoruiSUN")

{-# NOINLINE glGlobalAlphaFactoruiSUNFunPtr #-}

-- | Usage: @'glGlobalAlphaFactorusSUN' factor@


glGlobalAlphaFactorusSUN :: MonadIO m => GLushort -> m ()
glGlobalAlphaFactorusSUN = ffiushortIOV glGlobalAlphaFactorusSUNFunPtr

glGlobalAlphaFactorusSUNFunPtr :: FunPtr (GLushort -> IO ())
glGlobalAlphaFactorusSUNFunPtr = unsafePerformIO (getProcAddress "glGlobalAlphaFactorusSUN")

{-# NOINLINE glGlobalAlphaFactorusSUNFunPtr #-}

pattern GL_GLOBAL_ALPHA_FACTOR_SUN  :: (Eq a, Num a) => a

pattern GL_GLOBAL_ALPHA_FACTOR_SUN = 0x81DA

pattern GL_GLOBAL_ALPHA_SUN  :: (Eq a, Num a) => a

pattern GL_GLOBAL_ALPHA_SUN = 0x81D9