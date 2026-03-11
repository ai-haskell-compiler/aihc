{-|
Module: Numeric.Morpheus.Utils
Description: Utility Functions
Copyright: (c) Alexander Ignatyev, 2017
License: BSD-3
Stability: experimental
Portability: POSIX
-}

module Numeric.Morpheus.Utils
(
  morpheusLayout
)

where

import Foreign.C.Types

morpheusLayout :: CInt -> CInt -> CInt
morpheusLayout xCol cols = if xCol == 1 || cols == 1 then 21 else 22
