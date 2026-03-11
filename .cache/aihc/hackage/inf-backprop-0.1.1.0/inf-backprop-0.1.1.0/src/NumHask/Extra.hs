{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Module    :  NumHask.Extra
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Additional orphan instances for
-- [mumhusk](https://hackage.haskell.org/package/numhask)
-- typeclasses.
module NumHask.Extra () where

import NumHask (Additive, zero, (+))
import Prelude hiding (Num, (+))

instance {-# INCOHERENT #-} Additive () where
  (+) = const
  zero = ()

instance {-# INCOHERENT #-} (Additive x, Additive y) => Additive (x, y) where
  zero = (zero, zero)
  (a, b) + (c, d) = (a + c, b + d)

instance {-# INCOHERENT #-} (Additive x, Additive y, Additive z) => Additive (x, y, z) where
  zero = (zero, zero, zero)
  (x1, y1, z1) + (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

instance {-# INCOHERENT #-} (Additive x, Additive y, Additive z, Additive t) => Additive (x, y, z, t) where
  zero = (zero, zero, zero, zero)
  (x1, y1, z1, t1) + (x2, y2, z2, t2) = (x1 + x2, y1 + y2, z1 + z2, t1 + t2)

instance
  {-# INCOHERENT #-}
  (Additive x, Additive y, Additive z, Additive t, Additive s) =>
  Additive (x, y, z, t, s)
  where
  zero = (zero, zero, zero, zero, zero)
  (x1, y1, z1, t1, s1) + (x2, y2, z2, t2, s2) = (x1 + x2, y1 + y2, z1 + z2, t1 + t2, s1 + s2)
