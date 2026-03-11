-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.GREMEDY.StringMarker (
  -- * Extension Support
    gl_GREMEDY_string_marker

  -- * GL_GREMEDY_string_marker
  , glStringMarkerGREMEDY
) where

import Control.Monad.IO.Class
import Data.Set
import Foreign.Ptr
import Graphics.GL.Internal.FFI
import Graphics.GL.Internal.Proc
import Graphics.GL.Types
import System.IO.Unsafe

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/GREMEDY/string_marker.txt GL_GREMEDY_string_marker> extension is available.

gl_GREMEDY_string_marker :: Bool
gl_GREMEDY_string_marker = member "GL_GREMEDY_string_marker" extensions
{-# NOINLINE gl_GREMEDY_string_marker #-}

-- | Usage: @'glStringMarkerGREMEDY' len string@
--
-- The length of @string@ should be @len@.


glStringMarkerGREMEDY :: MonadIO m => GLsizei -> Ptr () -> m ()
glStringMarkerGREMEDY = ffisizeiPtrVIOV glStringMarkerGREMEDYFunPtr

glStringMarkerGREMEDYFunPtr :: FunPtr (GLsizei -> Ptr () -> IO ())
glStringMarkerGREMEDYFunPtr = unsafePerformIO (getProcAddress "glStringMarkerGREMEDY")

{-# NOINLINE glStringMarkerGREMEDYFunPtr #-}