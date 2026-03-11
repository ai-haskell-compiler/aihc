-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.APPLE.RowBytes (
  -- * Extension Support
    gl_APPLE_row_bytes

  -- * GL_APPLE_row_bytes
  , pattern GL_PACK_ROW_BYTES_APPLE
  , pattern GL_UNPACK_ROW_BYTES_APPLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://cvs.khronos.org/svn/repos/ogl/trunk/doc/registry/public/specs/APPLE/row_bytes.txt GL_APPLE_row_bytes> extension is available.

gl_APPLE_row_bytes :: Bool
gl_APPLE_row_bytes = member "GL_APPLE_row_bytes" extensions
{-# NOINLINE gl_APPLE_row_bytes #-}

pattern GL_PACK_ROW_BYTES_APPLE  :: (Eq a, Num a) => a

pattern GL_PACK_ROW_BYTES_APPLE = 0x8A15

pattern GL_UNPACK_ROW_BYTES_APPLE  :: (Eq a, Num a) => a

pattern GL_UNPACK_ROW_BYTES_APPLE = 0x8A16