module Demand (use) where

import GHC.TypeError (Assert, ErrorMessage (..), TypeError)
import GHC.Types (Bool (..))

-- The assertion fails, so the message constraint is demanded at the use
-- site and reported as the text it spells.
needs :: (Assert 'False (TypeError ('Text "cannot use " ':<>: 'ShowType Bool))) => Bool
needs = True

use :: Bool
use = needs
