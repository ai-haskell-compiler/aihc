module Numeric.CollectErrors
(
  -- * Type of numeric errors
  NumError(..), ErrorCertaintyLevel(..), NumErrors, CN, cn, unCN, unCNfn1, unCNfn2, (~!)
  -- * Utilities
, noValueNumErrorCertain, noValueNumErrorPotential
, removeValueErrorCertain, removeValueErrorPotential
, prependErrorCertain, prependErrorPotential
, CanClearPotentialErrors(..)
, CanTakeCNErrors
  -- ** Applicable general collect-error utilities
, noValue
, removeValue
, prependErrors
, CanTestErrorsCertain(..)
, CanTestErrorsPresent(..)
, toEither
, withErrorOrValue
, filterValuesWithoutError
, lift
, liftCN
, liftPair
, lift2
, lift2CN
, lift1T
, liftT1
, lift1TCN
, liftT1CN
, lift2pair
, lift1Tpair
, liftT1pair
, CanTakeErrors(..)
, liftTakeErrors
)
where

import Control.CollectErrors
import Numeric.CollectErrors.Type
import Numeric.CollectErrors.PreludeInstances ()
