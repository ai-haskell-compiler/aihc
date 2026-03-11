-- | 'Data.ListLike.ListLike' instances for several @Data.Vector@ types.
-- The @Data.ListLike.Vector.Generic@ instances are not exported from this
-- module in order to prevent collisions.
--
module Data.ListLike.Vector ()

where

import Data.ListLike.Vector.Storable ()
import Data.ListLike.Vector.Unboxed ()
import Data.ListLike.Vector.Vector ()
