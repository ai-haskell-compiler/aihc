module Whoops (Whoops) where

import GHC.TypeLits


-- | The constraint @Whoops s@ is unsatisfiable for every 'Symbol' @s@, as
-- in @Utils.Containers.Internal.TypeError@ of @containers@.
class Whoops (a :: Symbol)

instance TypeError ('Text a) => Whoops a
