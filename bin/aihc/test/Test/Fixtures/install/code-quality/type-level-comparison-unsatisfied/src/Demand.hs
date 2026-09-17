module Demand (use) where

import Data.Proxy (Proxy (..))
import GHC.Num.Integer (Integer)
import GHC.TypeLits (KnownNat, natVal, type (<=))

-- The comparison is a constraint synonym written infix. It is stuck here
-- and carried as a dictionary; at the use site it reduces to the
-- message it stands for, which is reported as GHC spells it.
atLeastOne :: forall n. (KnownNat n, 1 <= n) => Proxy n -> Integer
atLeastOne = natVal

use :: Integer
use = atLeastOne (Proxy :: Proxy 0)
