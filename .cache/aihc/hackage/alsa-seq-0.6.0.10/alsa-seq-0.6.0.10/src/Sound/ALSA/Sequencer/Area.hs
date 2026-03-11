--------------------------------------------------------------------------------
-- |
-- Module    : Sound.ALSA.Sequencer.Area
-- Copyright : (c) Henning Thielemann, 2010-2012
--             (c) Iavor S. Diatchki, 2007
-- License   : BSD3
--
-- Maintainer: Henning Thielemann
-- Stability : provisional
--
-- PRIVATE MODULE.
--
-- Here we have macros to deal with the various information
-- areas present in the library.
--------------------------------------------------------------------------------

module Sound.ALSA.Sequencer.Area (
  C(..),
  MU.with,
  MA.alloca,
  St.peek,
  Ptr.Ptr, Ptr.FunPtr,
  FPtr.ForeignPtr, FPtr.newForeignPtr, FPtr.withForeignPtr,
  CStr.CString, CStr.peekCString, CStr.withCAString,
  ) where

import qualified Foreign.Marshal.Utils as MU
import qualified Foreign.Marshal.Alloc as MA
import qualified Foreign.C.String as CStr
import qualified Foreign.Storable as St
import qualified Foreign.ForeignPtr as FPtr
import qualified Foreign.Ptr as Ptr


class C area where
   malloc :: IO area
   copy :: area -> area -> IO ()
   clone :: area -> IO area
