module NumericPrelude
   (module NumericPrelude.Numeric,
    module NumericPrelude.Base,
    max, min, abs, ) where

import NumericPrelude.Numeric hiding (abs, )
import NumericPrelude.Base    hiding (max, min, )
import Prelude ()
import Algebra.Lattice (max, min, abs, )
