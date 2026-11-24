-- | The 'Num' class, with the 'Integer' and 'Natural' operations.
--
-- This module has the same exports as the GHC module. Modules in this
-- package import "GHC.Internal.Num", because "GHC.Num.Integer" uses the
-- Prelude.
module GHC.Num
  ( Num (..),
    subtract,
    quotRemInteger,
    module GHC.Num.Integer,
    module GHC.Num.Natural,
  )
where

import GHC.Integer (quotRemInteger)
import GHC.Internal.Num (Num (..), subtract)
import GHC.Num.Integer
import GHC.Num.Natural
