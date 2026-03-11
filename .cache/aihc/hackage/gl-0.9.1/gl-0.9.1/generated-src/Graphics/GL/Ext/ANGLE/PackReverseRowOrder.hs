-- This file was automatically generated.
{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms #-}
module Graphics.GL.Ext.ANGLE.PackReverseRowOrder (
  -- * Extension Support
    gl_ANGLE_pack_reverse_row_order

  -- * GL_ANGLE_pack_reverse_row_order
  , pattern GL_PACK_REVERSE_ROW_ORDER_ANGLE
) where

import Data.Set
import Graphics.GL.Internal.Proc

-- | Checks that the <https://www.khronos.org/registry/gles/extensions/ANGLE/ANGLE_pack_reverse_row_order.txt GL_ANGLE_pack_reverse_row_order> extension is available.

gl_ANGLE_pack_reverse_row_order :: Bool
gl_ANGLE_pack_reverse_row_order = member "GL_ANGLE_pack_reverse_row_order" extensions
{-# NOINLINE gl_ANGLE_pack_reverse_row_order #-}

pattern GL_PACK_REVERSE_ROW_ORDER_ANGLE  :: (Eq a, Num a) => a

pattern GL_PACK_REVERSE_ROW_ORDER_ANGLE = 0x93A4