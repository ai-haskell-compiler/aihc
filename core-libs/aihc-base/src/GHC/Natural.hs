-- | The legacy interface to 'Natural'.  Only the part that packages use is
-- here.  New code should use "GHC.Num.Natural".
module GHC.Natural
  ( BigNat (..),
    Natural,
  )
where

import GHC.Num.BigNat (BigNat (..))
import GHC.Num.Natural (Natural)
