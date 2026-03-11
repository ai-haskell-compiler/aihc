module Control.CollectErrors
(
-- * Monad for collecting errors in expressions
  CollectErrors(..)
, CanBeErrors
, CanTestErrorsCertain(..)
, CanTestErrorsPresent(..)
-- ** Utilities
, noValue
, removeValue
, prependErrors
, unCollectErrors
, (~!)
, toEither
, withErrorOrValue
, filterValuesWithoutError
, lift
, liftCE
, liftPair
, lift2
, lift2CE
, lift1T
, liftT1
, lift1TCE
, liftT1CE
, lift2pair
, lift1Tpair
, liftT1pair
, CanTakeErrors(..)
, liftTakeErrors
)
where

import Control.CollectErrors.Type
import Control.CollectErrors.PreludeInstances ()
