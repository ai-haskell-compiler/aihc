{-# LANGUAGE CApiFFI #-}

module System.CPUTime.Clock (clock) where

import Foreign.C.Types (CClock (..))
import Prelude (IO)

foreign import capi unsafe "time.h clock"
  clock :: IO CClock
