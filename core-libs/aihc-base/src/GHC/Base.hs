-- | The GHC.Base API.
--
-- "GHC.Internal.Base" holds the definitions.
-- This module also re-exports 'Semigroup', 'Monoid', and 'NonEmpty', as GHC does.
-- "Data.Semigroup.Internal" imports "GHC.Internal.Base", thus the definitions cannot be here.
module GHC.Base
  ( module GHC.Internal.Base,
    Semigroup (..),
    Monoid (..),
    NonEmpty (..),
  )
where

import Data.Semigroup.Internal (Monoid (..), Semigroup (..))
import GHC.Internal.Base
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
