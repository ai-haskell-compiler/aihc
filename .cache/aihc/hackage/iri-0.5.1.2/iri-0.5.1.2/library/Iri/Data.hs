-- |
-- References:
--
-- * <https://tools.ietf.org/rfc/rfc3986.txt URI RFC>
-- * <https://tools.ietf.org/rfc/rfc3987.txt IRI RFC>
module Iri.Data
  ( module Iri.Data.Types,
    module Iri.Data.Functions,
  )
where

import Iri.Data.Functions
import Iri.Data.Instances.Eq ()
import Iri.Data.Instances.Generic ()
import Iri.Data.Instances.Hashable ()
import Iri.Data.Instances.Lift ()
import Iri.Data.Instances.Ord ()
import Iri.Data.Instances.Show ()
import Iri.Data.Types
