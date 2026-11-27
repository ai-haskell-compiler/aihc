module Repro.Capi (twice) where

import Foreign.C.Types (CInt (..))

foreign import capi unsafe "repro.h repro_twice" twice :: CInt -> CInt
