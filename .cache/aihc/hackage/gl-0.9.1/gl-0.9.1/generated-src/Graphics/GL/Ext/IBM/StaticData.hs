-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.IBM.StaticData (
  -- * Extension Support
    gl_IBM_static_data

  -- * GL_IBM_static_data
  , glFlushStaticDataIBM
  , pattern GL_ALL_STATIC_DATA_IBM
  , pattern GL_STATIC_VERTEX_ARRAY_IBM
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/IBM/static_data.txt GL_IBM_static_data> extension is available.

gl_IBM_static_data :: Bool
gl_IBM_static_data = member "GL_IBM_static_data" extensions
{-# NOINLINE gl_IBM_static_data #-}

-- | Usage: @'glFlushStaticDataIBM' target@


glFlushStaticDataIBM :: MonadIO m => GLenum -> m ()
glFlushStaticDataIBM = ffienumIOV glFlushStaticDataIBMFunPtr

glFlushStaticDataIBMFunPtr :: FunPtr (GLenum -> IO ())
glFlushStaticDataIBMFunPtr = unsafePerformIO (getProcAddress "glFlushStaticDataIBM")

{-# NOINLINE glFlushStaticDataIBMFunPtr #-}

pattern GL_ALL_STATIC_DATA_IBM  :: (Eq a, Num a) => a

pattern GL_ALL_STATIC_DATA_IBM = 103060

pattern GL_STATIC_VERTEX_ARRAY_IBM  :: (Eq a, Num a) => a

pattern GL_STATIC_VERTEX_ARRAY_IBM = 103061